package uk.gov.gds.model

import uk.gov.gds.model.CodeLists.LogicalStatusCode
import com.mongodb.casbah.commons.MongoDBObject
import com.novus.salat._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._


case class Address(line1: String, line2: String, street: Option[String], town: Option[String], locality: Option[String], area: Option[String], postcode: String) {
  def asDbObject = {
    MongoDBObject(
      "line1" -> line1,
      "line2" -> line2,
      "street" -> street,
      "town" -> town,
      "locality" -> locality,
      "area" -> area,
      "postcode" -> postcode
    )
  }

  def serialize =  grater[Address].asDBObject(this)
}

object AddressBuilder {


  def geographicAddressToSimpleAddress(addressWrapper: AddressBaseWrapper, streets: Map[String, List[Street]] = Map.empty[String, List[Street]], streetDescriptors: Map[String, StreetDescriptor] = Map.empty[String, StreetDescriptor]) = {

   // if (addressWrapper.lpis.size > 1) println("too big " + addressWrapper.blpu.uprn)

    val lpis = filerOutIneligibleLPIs(addressWrapper.lpis)

    if (lpis.size <= 0) {
      println("No eligible LPI for " + addressWrapper.blpu.uprn)
      None
    } // No LPI for this address
    else if (lpis.size > 1) throw new Exception("too many eligble LPIs for " + addressWrapper.blpu.uprn) // Too many LPIs (error state)
    else if (!addressWrapper.blpu.logicalState.get.equals(LogicalStatusCode.approved)) None // BLPU is not approved
    else if (!streetDescriptors.contains(lpis.head.usrn)) None // No streets for the LPI
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

      if (streetDescriptors.contains(lpis.head.usrn)) {
        val street = streetDescriptors(lpis.head.usrn).streetDescription
        val town = streetDescriptors(lpis.head.usrn).townName
        val area = streetDescriptors(lpis.head.usrn).administrativeArea
        val locality = streetDescriptors(lpis.head.usrn).localityName
        Some(Address(sao, pao, street, town, locality, area, addressWrapper.blpu.postcode))
      } else Some(Address(sao, pao, None, None, None, None, addressWrapper.blpu.postcode))
    }
  }

  // TODO - filter on:
  // enddate
  // approved
  // historical

  private def filerOutIneligibleLPIs(lpis: List[LPI]) = lpis.filter(lpi => !lpi.endDate.isDefined && lpi.logicalState.get.equals(LogicalStatusCode.approved))

  private def constructStringFromListOfStringOptions(strings: List[Option[String]]) = strings.flatten.mkString(" ")

}
