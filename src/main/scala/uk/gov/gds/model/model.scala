package uk.gov.gds.model

sealed abstract class Outcome(r: Boolean)

case object Success extends Outcome(true)

case object Failure extends Outcome(false)

case class Result(outcome: Outcome, message: String)





/* Basic Land and Property Unit */
case class BLPU(
                 uprn: String,
                 state: BlpuStateCode

                 )

object BLPU {
  val recordIdentifier = 21
}




