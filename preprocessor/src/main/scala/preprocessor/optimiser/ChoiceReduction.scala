package preprocessor.optimiser

import preprocessor.optimiser.IRNodes._
import Utils._

object ChoiceReduction extends PipelineStage {

  override def processIR(ir: IR)(implicit state: IRState): IR = {
    val reduced = super.processIR(ir)
    l2c(c2l(reduced).distinct)
  }

  private def l2c(is: List[IR]): IR = is.reduce(Choice)
}
