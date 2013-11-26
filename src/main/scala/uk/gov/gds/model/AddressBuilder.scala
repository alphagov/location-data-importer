package uk.gov.gds.model

import uk.gov.gds.model.CodeLists.{BlpuStateCode, LogicalStatusCode}
import com.novus.salat._
import com.novus.salat.global._
import uk.gov.gds.logging.Logging
import org.joda.time.DateTime


case class Location(x: Double, y: Double)

case class Details(
                    blpuCreatedAt: DateTime,
                    blpuUpdatedAt: DateTime,
                    classification: String,
                    status: Option[String] = None,
                    state: Option[String] = None,
                    isPostalAddress: Boolean,
                    isCommercial: Boolean,
                    isResidential: Boolean
                    )

case class Presentation(
                         property: Option[String] = None,
                         streetAddress: Option[String],
                         locality: Option[String] = None,
                         town: Option[String] = None,
                         area: Option[String] = None,
                         postcode: String,
                         uprn: String
                         )

case class Address(
                    houseNumber: Option[String],
                    houseName: Option[String],
                    postcode: String,
                    gssCode: String,
                    createdAt: DateTime = new DateTime,
                    presentation: Presentation,
                    location: Location,
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

      val streetPrefix = constructStreetAddressPrefix(lpi)

      val street = streetDescriptors(lpi.usrn).streetDescription
      val locality = streetDescriptors(lpi.usrn).localityName
      val town = streetDescriptors(lpi.usrn).townName
      val area = if (
        town.isDefined &&
          !town.equals(streetDescriptors(lpi.usrn).administrativeArea)) {
        Some(streetDescriptors(lpi.usrn).administrativeArea)
      } else {
        None
      }



      val location = Location(addressWrapper.blpu.xCoordinate, addressWrapper.blpu.yCoordinate)
      val presentation = Presentation(
        property = constructProperty(lpi),
        streetAddress = String.format("%s %s", streetPrefix, street).trim,
        locality = locality,
        town = town,
        area = area,
        postcode = addressWrapper.blpu.postcode,
        uprn = addressWrapper.uprn
      )


      val details = Details(
        blpuCreatedAt = addressWrapper.blpu.startDate,
        blpuUpdatedAt = addressWrapper.blpu.lastUpdated,
        classification = addressWrapper.classifications.head.classificationCode,
        status = addressWrapper.blpu.blpuState.map(pp => pp.toString),
        state = addressWrapper.blpu.logicalState.map(pp => pp.toString),
        isPostalAddress = addressWrapper.blpu.canReceivePost,
        isResidential = addressWrapper.classifications.head.isResidential,
        isCommercial = !addressWrapper.classifications.head.isResidential
      )

      Some(Address(
        houseName = lpis.head.paoText,
        houseNumber = Some(streetPrefix),
        gssCode = addressWrapper.blpu.localCustodianCode,
        postcode = addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", ""),
        presentation = presentation,
        location = location,
        details = details
      ))


    }
  }

  private def filerOutIneligibleLPIs(lpis: List[LPI]) = lpis.filter(lpi => !lpi.endDate.isDefined && lpi.logicalState.get.equals(LogicalStatusCode.approved))

  def isValidBLPU(blpu: BLPU) = !List(
    blpu.logicalState.getOrElse(false).equals(LogicalStatusCode.approved), // MUST have a logical state and it MUST be 'approved'
    blpu.blpuState.getOrElse(false).equals(BlpuStateCode.inUse), // MUST have a BLPU state and it MUST be 'in use'
    !blpu.endDate.isDefined, // MUST not have an end date
    blpu.canReceivePost // must be able to receive post
  ).contains(false)

  def constructProperty(lpi: LPI) = {
    val formatted = List(formatStartAndEndSuffixes(lpi.saoStartNumber, lpi.saoStartSuffix, lpi.saoEndNumber, lpi.saoEndSuffix), lpi.saoText, lpi.paoText).flatten.mkString(" ")
    if (formatted.isEmpty) None
    else Some(formatted)
  }

  def constructStreetAddressPrefix(lpi: LPI) = List(formatStartAndEndSuffixes(lpi.paoStartNumber, lpi.paoStartSuffix, lpi.paoEndNumber, lpi.paoEndSuffix)).flatten.mkString(" ")

  private def formatStartAndEndSuffixes(startNumber: Option[String], startSuffix: Option[String], endNumber: Option[String], endSuffix: Option[String]) = {
    val start = if (startNumber.isDefined) List(startNumber, startSuffix).flatten.mkString("") else ""
    val end = if (endNumber.isDefined) List(endNumber, endSuffix).flatten.mkString("") else ""

    if (!start.isEmpty && !end.isEmpty) Some(start + "-" + end)
    else if (!start.isEmpty && end.isEmpty) Some(start)
    else None
  }
}
