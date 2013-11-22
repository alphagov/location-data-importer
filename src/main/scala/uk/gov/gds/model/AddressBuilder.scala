package uk.gov.gds.model

import uk.gov.gds.model.CodeLists.{BlpuStateCode, LogicalStatusCode}
import com.mongodb.casbah.commons.MongoDBObject
import com.novus.salat._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import uk.gov.gds.logging.Logging
import uk.gov.gds.model.CodeLists.StreetStateCode.StreetStateCode

case class Details(
                    classificationCode: String,
                    blpuLogicalStatus: Option[String] = None,
                    blpuStatus: Option[String] = None,
                    streetState: Option[String] = None,
                    streetSurface: Option[String] = None,
                    streetClassification: Option[String] = None
                    )

object Details {
  def apply(addressWrapper: AddressBaseWrapper, streets: List[Street]) = {
    val classificationCode = addressWrapper.classifications.filter(c => !c.endDate.isDefined).head.classificationCode
    val s = streets.filter(s => !s.endDate.isDefined).head
    new Details(
      classificationCode,
      addressWrapper.blpu.logicalState.map(pp => pp.toString),
      addressWrapper.blpu.blpuState.map(pp => pp.toString),
      s.state.map(pp => pp.toString),
      s.surface.map(pp => pp.toString),
      s.classification.map(pp => pp.toString))
  }
}

case class Address(property: Option[String] = None,
                   streetAddress: String,
                   town: String,
                   locality: Option[String] = None,
                   area: Option[String] = None,
                   postcode: String,
                   lcPostcode: String,
                   localCustodianCode: Int,
                   uprn: String,
                   details: Details
                    ) {
  def serialize = grater[Address].asDBObject(this)
}


object AddressBuilder extends Logging {


  def geographicAddressToSimpleAddress(addressWrapper: AddressBaseWrapper, streets: Map[String, List[Street]] = Map.empty[String, List[Street]], streetDescriptors: Map[String, StreetDescriptor] = Map.empty[String, StreetDescriptor]) = {

    val lpis = filerOutIneligibleLPIs(addressWrapper.lpis)

    if (!isValidBLPU(addressWrapper.blpu)) {
      logger.info("Not eligible BLPU " + addressWrapper.blpu.uprn)
      None
    }
    else if (lpis.size <= 0) {
      logger.info("No eligible LPI for " + addressWrapper.blpu.uprn)
      None
    } // No LPI for this address
    else if (lpis.size > 1) {
      logger.info("too many eligble LPIs for " + addressWrapper.blpu.uprn)
      None
    }
    else if (!streetDescriptors.contains(lpis.head.usrn)) {
      logger.info("no street descriptors for " + addressWrapper.blpu.uprn)
      None
    }
    else if (!streets.contains(lpis.head.usrn)) {
      logger.info("no streets for " + addressWrapper.blpu.uprn)
      None
    }
    else {
      val lpi = lpis.head

      val streetPrefix = constructStreetAddress(lpi)

      val street = streetDescriptors(lpi.usrn).streetDescription
      val locality = streetDescriptors(lpi.usrn).localityName
      val town = streetDescriptors(lpi.usrn).townName
      val area = streetDescriptors(lpi.usrn).administrativeArea

      val address = Address(
        property = constructProperty(lpi),
        streetAddress = String.format("%s %s",streetPrefix,street).trim,
        town = area,
        locality = locality,
        postcode = addressWrapper.blpu.postcode,
        lcPostcode = addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ",""),
        uprn = addressWrapper.uprn,
        localCustodianCode = addressWrapper.blpu.localCustodianCode,
        details = Details(addressWrapper, streets(lpi.usrn))
      )

      if (town.isDefined && !town.equals(area)) Some(address.copy(town = town.get, area = area))
      else Some(address)
    }
  }

  private def filerOutIneligibleLPIs(lpis: List[LPI]) = lpis.filter(lpi => !lpi.endDate.isDefined && lpi.logicalState.get.equals(LogicalStatusCode.approved))

  def isValidBLPU(blpu: BLPU) = !List(
    blpu.logicalState.getOrElse(false).equals(LogicalStatusCode.approved), // MUST have a logical state and it MUST be 'approved'
    blpu.blpuState.getOrElse(false).equals(BlpuStateCode.inUse), // MUST have a BLPU state and it MUST be 'in use'
    !blpu.endDate.isDefined, // MUST not have an end date
    blpu.canReceivePost // must be able to recieve post
  ).contains(false)

  def constructProperty(lpi: LPI) = {
    val formatted = List(formatStartAndEndSuffixes(lpi.saoStartNumber, lpi.saoStartSuffix, lpi.saoEndNumber, lpi.saoEndSuffix), lpi.saoText, lpi.paoText).flatten.mkString(" ")
    if (formatted.isEmpty) None
    else Some(formatted)
  }

  def constructStreetAddress(lpi: LPI) = List(formatStartAndEndSuffixes(lpi.paoStartNumber, lpi.paoStartSuffix, lpi.paoEndNumber, lpi.paoEndSuffix)).flatten.mkString(" ")

  private def formatStartAndEndSuffixes(startNumber: Option[String], startSuffix: Option[String], endNumber: Option[String], endSuffix: Option[String]) = {
    val start = if (startNumber.isDefined) List(startNumber, startSuffix).flatten.mkString("") else ""
    val end = if (endNumber.isDefined) List(endNumber, endSuffix).flatten.mkString("") else ""

    if (!start.isEmpty && !end.isEmpty) Some(start + "-" + end)
    else if (!start.isEmpty && end.isEmpty) Some(start)
    else None
  }
}
