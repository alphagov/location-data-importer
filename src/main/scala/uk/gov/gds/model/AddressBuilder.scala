package uk.gov.gds.model

import uk.gov.gds.model.CodeLists.LogicalStatusCode

case class Address(line1: String, line2: String, postcode: String)

object AddressBuilder {


  def geographicAddressToSimpleAddress(addressWrapper: AddressBaseWrapper) = {

    if (addressWrapper.lpis.size > 1) println("too big " + addressWrapper.blpu.uprn)

    val lpis = filerOutIneligibleLPIs(addressWrapper.lpis)

    if (lpis.size <= 0) {
      println("No eligible LPI for " + addressWrapper.blpu.uprn)
      None
    }
    else if (lpis.size > 1) throw new Exception("too many eligble LPIs for " + addressWrapper.blpu.uprn)
    else if(!addressWrapper.blpu.logicalState.get.equals(LogicalStatusCode.approved)) None
    else {
      val sao = constructStringFromListOfStringOptions(
        List(lpis.head.saoText,
          lpis.head.saoStartNumber,
          lpis.head.saoStartSuffix,
          lpis.head.saoEndNumber,
          lpis.head.saoEndSuffix))

      val pao = constructStringFromListOfStringOptions(
        List(lpis.head.paoText,
          lpis.head.paoStartNumber,
          lpis.head.paoStartSuffix,
          lpis.head.paoEndNumber,
          lpis.head.paoEndSuffix))

      Some(Address(sao, pao, addressWrapper.blpu.postcode))
    }
  }

  // TODO - filter on:
  // enddate
  // approved
  // historical

  private def filerOutIneligibleLPIs(lpis: List[LPI]) = lpis.filter(lpi => !lpi.endDate.isDefined && lpi.logicalState.get.equals(LogicalStatusCode.approved))

  private def constructStringFromListOfStringOptions(strings: List[Option[String]]) = strings.flatten.mkString(" ")

}
