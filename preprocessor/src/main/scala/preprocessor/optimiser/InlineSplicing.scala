package preprocessor.optimiser

import preprocessor.parser.ASTNodes._

import scala.collection.mutable

object InlineSplicing extends PipelineStage {

  type Inline = Either[Expression, (List[String], Expression)]
  type DeclarationMap = mutable.Map[String, Inline]

  override def process(m: Module)(implicit state: IRState): Module = {
    val inlineDeclarations = mutable.Map.empty[String, Inline]
    val dependencies = mutable.Map.empty[String, mutable.Set[String]]
    val recursive = mutable.Set.empty[String]

    val mm = new ScanStep(inlineDeclarations).run(m)

    inlineDeclarations.foreach { case name -> id =>
      val deps = mutable.Set.empty[String]
      new DependencyStep(inlineDeclarations, deps).processE(id match {
        case Left(e) => e
        case Right((_, e)) => e
      })
      dependencies.addOne(name, deps)
    }

    dependencies.foreach { case l -> _ =>
      val seen = mutable.Set.empty[String]

      def visit(n: String): Boolean = {
        if (seen.contains(n)) {
          true
        } else {
          seen.add(n)
          dependencies.getOrElse(n, Set()).exists(visit)
        }
      }

      if (visit(l)) {
        recursive.add(l)
      }
    }

    if (recursive.nonEmpty) {
      state.addMessage(f"InlineSplicing: Some definitions contain local / mutual recursion:")
      recursive.foreach { r => state.addMessage(f"  - $r") }
    }

    new ExpandStep(inlineDeclarations, recursive).process(mm)
  }

  private class DependencyStep(val dMap: DeclarationMap, val acc: mutable.Set[String]) extends PipelineStage {
    override def processE(e: Expression)(implicit state: IRState): Expression = {
      e match {
        case Identifier(name) if dMap.contains(name) => acc.add(name)
        case _ => ()
      }
      super.processE(e)
    }
  }

  private class ScanStep(val dMap: DeclarationMap) extends PipelineStage {
    override def processStat(s: Statement)(implicit state: IRState): Statement = s match {
      case InlineVariableStatement(Identifier(name), _, rhs) =>
        dMap.addOne(name, Left(processE(rhs)))
        EmptyStatement
      case InlineFunctionStatement(Identifier(name), params, rhs) =>
        dMap.addOne(name, Right((params.map(_.name), processE(rhs))))
        EmptyStatement
      case _ => super.processStat(s)
    }
  }

  private class ExpandStep(val dMap: DeclarationMap, val recursion: mutable.Set[String]) extends PipelineStage {
    override def processE(e: Expression)(implicit state: IRState): Expression = {
      e match {
        case i@Identifier(name) if !recursion.contains(name) => dMap.get(name) match {
          case Some(Left(value)) => processE(value)
          case _ => super.processE(i)
        }
        case mc@MemberCall(Identifier(name), Arguments(_, args)) if !recursion.contains(name) => dMap.get(name) match {
          case Some(Right((params, e))) if args.length == params.length =>
            val mapping = (params zip args.map(processE)).toMap
            val stage = new InnerExpandStep(mapping)

            processE(stage.processE(e))
          case _ => super.processE(mc)
        }
        case _ => super.processE(e)
      }
    }
  }

  private class InnerExpandStep(val mapping: Map[String, Expression]) extends PipelineStage {
    override def processE(e: Expression)(implicit state: IRState): Expression = e match {
      case i@Identifier(name) => mapping.get(name) match {
        case Some(value) => value
        case None => i
      }
      case _ => super.processE(e)
    }
  }
}
