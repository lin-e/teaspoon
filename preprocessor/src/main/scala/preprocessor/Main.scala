package preprocessor

import parsley._
import parser.ASTNodes._
import parser.TypeScript
import preprocessor.optimiser.IRState
import scopt.OParser
import java.io.File
import java.io.PrintWriter

import scala.io.Source
import scala.sys.process.Process
import scala.sys.process.ProcessLogger

object Main {
  type ErrorOr[T] = Either[String, T]

  case class Config(implicitCS: Boolean = false,
                    choiceReduction: Boolean = false,
                    leftRecursionAnalysis: Boolean = false,
                    stringTrie: Boolean = false,
                    backtrackingReduction: Boolean = false,
                    parserLawOpt: Boolean = false,
                    globalFirstSet: Boolean = true,
                    testing: Boolean = false,
                    input: File = new File("."),
                    output: File = new File("."),
                    prettier: Boolean = false,
                   )

  def main(args: Array[String]): Unit = {
    val optParser = {
      val builder = OParser.builder[Config]

      import builder._
      OParser.sequence(
        programName("teaspoon"),
        head("TypeScript(ish) Preprocessor"),
        arg[File]("<file>")
          .action((x, c) => c.copy(input = x))
          .text("Path to source (original) file")
          .required(),
        opt[Unit]('x', "testing")
          .hidden()
          .action((_, c) => c.copy(testing = true)),
        help('h', "help")
          .text("Displays this message"),
//        opt[File]('f', "in")
//          .action((x, c) => c.copy(input = x))
//          .required(),
        opt[File]('o', "out")
          .action((x, c) => c.copy(output = x))
          .text("Path to target (new) file")
          .required(),
        opt[Unit]('p', "prettier")
          .action((_, c) => c.copy(prettier = true))
          .text("Reformats the generated code (requires 'prettier')"),
        note(""),
        note("OPTIMISATIONS"),
        opt[Unit]('a', "all")
          .action((_, c) => c.copy(implicitCS = true))
          .action((_, c) => c.copy(choiceReduction = true))
          .action((_, c) => c.copy(leftRecursionAnalysis = true))
          .action((_, c) => c.copy(stringTrie = true))
          .action((_, c) => c.copy(backtrackingReduction = true))
          .action((_, c) => c.copy(parserLawOpt = true))
          .text("Enable all optimisations"),
        opt[Unit]("implicit-cs-conversion")
          .abbr("icc")
          .action((_, c) => c.copy(implicitCS = true))
          .text("Implicitly convert chars / strings when used directly with a combinator"),
        opt[Unit]("choice-reduction")
          .abbr("cr")
          .action((_, c) => c.copy(choiceReduction = true))
          .text("Remove redundant alternatives"),
        opt[Unit]("left-recursion-analysis")
          .abbr("lra")
          .action((_, c) => c.copy(leftRecursionAnalysis = true))
          .text("Analyse and attempt remediation of left-recursive productions"),
        opt[Unit]("string-trie")
          .abbr("st")
          .action((_, c) => c.copy(stringTrie = true))
          .text("Analyse string choice chains and optimise"),
        opt[Unit]("backtracking-reduction")
          .abbr("br")
          .action((_, c) => c.copy(backtrackingReduction = true))
          .text("Analyse attempt chains and optimise to reduce backtracking by factoring"),
        opt[Unit]("parser-law-optimisation")
          .abbr("plo")
          .action((_, c) => c.copy(parserLawOpt = true))
          .text("Basic optimisations using parser laws"),
        opt[Unit]("no-global-first-set")
          .abbr("ngfs")
          .action((_, c) => c.copy(globalFirstSet = false))
          .text("Restrict first set analysis to local declaration (global by default)"),
      )
    }

    OParser.parse(optParser, args, Config()) match {
      case Some(cfg) => process(cfg)
      case None =>
    }
  }

  def process(cfg: Config): Unit = {
    val parser = TypeScript.module
    val (pipeline, state) = createPipeline(cfg)

    val input = if (cfg.testing) {
      println("TESTING MODE")
      println(f"CONFIG: $cfg")
      val thesisExamples =
        """
          |                val  nat  = parseInt <$> /[0-9]+/;
          |                lazy expr = expr <**> ('+' $> add) <*> term <|>
          |                            expr <**> ('-' $> sub) <*> term <|>
          |                            term;
          |                lazy term = term <**> ('*' $> mul) <*> fact <|>
          |                            term <**> ('/' $> div) <*> fact <|>
          |                            fact;
          |                lazy fact = nat <|>
          |                            '(' *> expr <* ')';
          |""".stripMargin

      Right(thesisExamples)
    } else {
      safeRead(cfg.input)
    }

    val result = for {
      in <- input
      ast <- parse(parser, in)
      opt <- runPipeline(pipeline, ast)
      output <- if (cfg.testing) {
        val (bPipe, _) = barePipeline()
        val processed = runPipeline(bPipe, ast) match {
          case Left(_) => ast // failed
          case Right(v) => v
        }
        dump(ast, processed, opt)
        Right("TESTING COMPLETE")
      } else {
        for {
          _ <- safeWrite(cfg.output, opt.print())
          _ <- if (cfg.prettier) {
            safePretty(cfg.output.getAbsolutePath)
          } else {
            Right(())
          }
          result <- safeRead(cfg.output)
        } yield result
      }
    } yield output

    result match {
      case Left(msg) => println(msg)
      case Right(prog) => println(prog)
    }

    state.getMessages.foreach(println)
  }

  private def dump(ast: Module, processed: Module, optimised: Module): Unit = {
    println("-----------------------------")
    println(f"AST:       $ast")
    println("-----------------------------")
    println(f"PROCESSED: $processed")
    println("-----------------------------")
    println(f"OPTIMISED: $optimised")
    println("-----------------------------")
    println(
      f"""REWRITTEN (PRETTIER):
         |${prettier(processed.print())}
         |""".stripMargin)
    println("-----------------------------")
    println(
      f"""OPTIMISED:
         |${prettier(optimised.print())}
         |""".stripMargin)
    println("-----------------------------")
  }

  private def safeRead(f: File): ErrorOr[String] = {
    try {
      val src = Source.fromFile(f)
      try {
        Right(src.mkString)
      } finally {
        src.close()
      }
    } catch {
      case e: Throwable => Left(f"Failed to read ${f.getAbsolutePath}: ${e.getMessage}")
    }
  }

  private def safeWrite(f: File, contents: String): ErrorOr[Unit] = {
    try {
      val p = new PrintWriter(f)
      try {
        p.println(contents)
        Right(())
      } finally {
        p.close()
      }
    } catch {
      case e: Throwable => Left(f"Failed to write ${f.getAbsolutePath}: ${e.getMessage}")
    }
  }

  private def safePretty(path: String): ErrorOr[Unit] = {
    try {
      val cmd = Seq("prettier", path, "--write")
      Process(cmd) ! ProcessLogger(_ => ())
      Right(())
    } catch {
      case e: Throwable => Left(f"Failed to reformat: ${e.getMessage}")
    }
  }

  private def barePipeline(): (Module => Module, IRState) = {
    implicit val state: IRState = new IRState()
    val pipeline = Seq[Module => Module](
      optimiser.InlineSplicing.run,
      optimiser.ToIR.run,
      optimiser.FromIR.run,
      optimiser.LanguageExtensions.run,
    ).reduce(_ andThen _)

    (pipeline, state)
  }

  private def createPipeline(cfg: Config): (Module => Module, IRState) = {
    implicit val state: IRState = new IRState()

    val stages = Seq[Module => Module](
      optimiser.InlineSplicing.run,
      optimiser.ImplicitConversion.run,
      optimiser.ToIR.run,
      optimiser.GlobalFirstSetComputation.run,
      optimiser.ParserLawOptimisation.run,
      optimiser.ChoiceReduction.run,
      optimiser.LeftRecursionAnalysis.run,
      optimiser.StringChoiceTrie.run,
      optimiser.BacktrackingReduction.run,
      optimiser.FromIR.run,
      optimiser.LanguageExtensions.run,
    )

    val toggles = Seq(
      true,
      cfg.implicitCS,
      true,
      cfg.globalFirstSet,
      cfg.parserLawOpt,
      cfg.choiceReduction,
      cfg.leftRecursionAnalysis,
      cfg.stringTrie,
      cfg.backtrackingReduction,
      true,
      true,
    )

    assert(stages.length == toggles.length)

    val pipeline = (stages zip toggles)
      .flatMap { case (stage, toggle) => if (toggle) Some(stage) else None }
      .reduce(_ andThen _)

    (pipeline, state)
  }

  private def parse(parser: Parsley[Module], input: String): ErrorOr[Module] = {
    parser.parse(input) match {
      case Success(x) => Right(x)
      case Failure(msg) => Left(f"Failed to parse: $msg")
    }
  }

  private def runPipeline(pipeline: Module => Module, input: Module): ErrorOr[Module] = {
    try {
      Right(pipeline(input))
    } catch {
      case e: Throwable => Left(f"Failed to run pipeline: ${e.getMessage}")
    }
  }

  // For debugging use mainly; this will be in the pipeline rather than in the preprocessor
  private def prettier(in: String, path: String = "prettier/out.ts", cfg: String = "prettier/.prettierrc"): String = {
    import java.io._

    import scala.io._
    import scala.language.postfixOps
    import sys.process._

    val p = new PrintWriter(new File(path))
    try p.println(in) finally p.close()

    val cmd = Seq("./prettier/prettier.sh", cfg, path)
    // hacky
    val env = "PATH" -> "/home/line/.nvm/versions/node/v16.13.1/bin"

    Process(cmd, None, env) ! ProcessLogger(_ => ())

    val src = Source.fromFile(path)
    try src.getLines().mkString("\n") finally src.close()
  }
}
