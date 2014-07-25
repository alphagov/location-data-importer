package uk.gov.gds.location.importer.conversions

import uk.gov.gds.location.importer.logging._
import uk.gov.gds.location.importer.model._
import uk.gov.gds.location.importer.model.CodeLists.StreetRecordTypeCode
import uk.gov.gds.location.importer.model.LocalAuthorities._
import uk.gov.gds.location.importer.model.Location
import scala.Some
import uk.gov.gds.location.importer.model.Details
import uk.gov.gds.location.importer.model.OrderingHelpers
import uk.gov.gds.location.importer.model.StreetWithDescription
import uk.gov.gds.location.importer.model.AddressBaseWrapper
import uk.gov.gds.location.importer.model.Presentation
import uk.gov.gds.location.importer.model.Address
import uk.gov.gds.location.importer.encryption.AesEncryptionService

/**
 * Object to take lists of address base types and convert to our address / street model
 */
object AddressBaseToLocateConvertor extends Logging {

  import formatters._

  implicit private def stringToOptionalInt(v: String): Option[Int] = {
    try {
      Some(Integer.valueOf(v))
    } catch {
      case e: Exception => None
    }
  }

  def toLocateAddress(addressWrapper: AddressBaseWrapper, fileName: String) = {
    val streetDescriptor = AllTheStreets.allTheStreets.get(addressWrapper.lpi.usrn)

    streetDescriptor match {
      case Some(street) => {
        findGssCodeFrom(addressWrapper) match {
          case Some(gssCode) => address(addressWrapper, gssCode, street, fileName)
          case _ =>
            logger.error(String.format("FAILED [No local authority found for address]: BLPU [%s] POSTCODE [%s] FILENAME [%s] custodian code [%s]", addressWrapper.uprn, addressWrapper.blpu.postcode, fileName, addressWrapper.blpu.localCustodianCode))
            None
        }
      }
      case _ =>
        logger.error(String.format("FAILED [No street found for address]: BLPU [%s] USRN [%s] POSTCODE [%s] FILENAME [%s] custodian [%s]", addressWrapper.uprn, addressWrapper.lpi.usrn, addressWrapper.blpu.postcode, fileName, addressWrapper.blpu.localCustodianCode))
        None
    }
  }

  /**
   * Establish gss code.
   * Firstly use custodian code to look in LocalAuthorities Class.
   * If no authority found use postcode to look in code points collections
   * If no gss code return Optional of None
   * @param addressWrapper
   * @return Option[String] gssCode
   */
  def findGssCodeFrom(addressWrapper: AddressBaseWrapper) = {
    localAuthoritiesByCustodianCode.get(addressWrapper.blpu.localCustodianCode) match {
      case Some(la) => Some(la.gssCode)
      case _ => AllTheCodePoints.codePoints.get(addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", "")) match {
        case Some(codePoint) => Some(codePoint._1)
        case _ => None
      }
    }
  }

  def address(addressWrapper: AddressBaseWrapper, gssCode: String, street: StreetWithDescription, fileName: String) = {
    val key = sys.props.get("key").get
    val ivSpec = AesEncryptionService.generateInitializationVector

    val address = Address(
      gssCode = gssCode,
      country = Countries.countryForGssCode(gssCode),
      uprn = addressWrapper.blpu.uprn,
      postcode = lowercase(stripAllWhitespace(addressWrapper.blpu.postcode)),
      presentation = presentation(addressWrapper.blpu, addressWrapper.lpi, street, addressWrapper.deliveryPoint, fileName).encrypted(key, ivSpec),
      location = location(addressWrapper.blpu),
      details = details(addressWrapper, fileName),
      ordering = Some(ordering(addressWrapper, street, fileName).encrypted(key, ivSpec)),
      iv = AesEncryptionService.byteArrayAsBase64String(ivSpec.getIV)
    )

    audit(address) match {
      case true => Some(address)
      case _ => {
        logger.error("FAILED [Audit]: " + address)
        None
      }
    }
  }

  /**
   * All addreses must have a number of key properties. Check and return false if any missing
   * @param a: Address
   */
  def audit(a: Address) = {
    def valid(s: String) = s != null && s.length > 0

    valid(a.postcode) &&
      valid(a.gssCode) &&
      valid(a.uprn) &&
      (valid(a.presentation.street.getOrElse("")) || valid(a.presentation.property.getOrElse(""))) &&
      valid(a.presentation.postcode) &&
      a.ordering.isDefined
  }

  /**
   * Address base objects have a custodian code, which maps to a GSSCode
   * Compare the Codepoint derived GSSCode with the Custodian code version
   * Using custodian code on any variation
   * @param custodianCode
   * @param gssCode
   * @param fileName
   * @return   
   */
  def checkGssCodeWithCustodianCode(blpu: BLPU, gssCode: String, fileName: String) = {
    localAuthoritiesByCustodianCode.get(blpu.localCustodianCode) match {
      case Some(la) if la.gssCode.equalsIgnoreCase(gssCode) => gssCode
      case Some(la) if !la.gssCode.equalsIgnoreCase(gssCode) => {
        logger.info("UPDATED [GSSCode and Custodian code mismatch]: file [%s] uprn [%s] custodian code[%s], la.gssCode [%s] codepoint.gssCode [%s]".format(fileName, blpu.uprn, blpu.localCustodianCode, la.gssCode, gssCode))
        la.gssCode
      }
      case _ => gssCode
    }
  }

  /*
    Model class builders
   */

  def ordering(addressWrapper: AddressBaseWrapper, street: StreetWithDescription, file: String) = OrderingHelpers(
    paoStartNumber = addressWrapper.lpi.paoStartNumber.flatMap(n => n),
    paoStartSuffix = addressWrapper.lpi.paoStartSuffix.flatMap(n => n),
    paoEndNumber = addressWrapper.lpi.paoEndNumber.flatMap(n => n),
    paoEndSuffix = addressWrapper.lpi.paoEndSuffix.flatMap(n => n),
    saoStartNumber = addressWrapper.lpi.saoStartNumber.flatMap(n => n),
    saoStartSuffix = addressWrapper.lpi.saoStartSuffix.flatMap(n => n),
    saoEndNumber = addressWrapper.lpi.saoEndNumber.flatMap(n => n),
    saoEndSuffix = addressWrapper.lpi.saoEndSuffix.flatMap(n => n),
    paoText = addressWrapper.lpi.paoText, //.map(v => stripAllWhitespace(lowercase(v))),
    saoText = addressWrapper.lpi.saoText, //.map(v => stripAllWhitespace(lowercase(v)))
    street = chooseStreetDescription(addressWrapper.lpi, street, addressWrapper.deliveryPoint, file).flatMap(n => n)
  )

  def location(blpu: BLPU) = Location(blpu.lat, blpu.long)

  /**
   * Construct the presentation object
   * Note in some cases the street that is made for an address may exactly overlap with area/town/locality
   * In this case go with the street and None the matching field
   * Happens in relativly rare case where street derived from PAF fields.
   * @param blpu
   * @param lpi
   * @param street
   * @param deliveryPoint
   * @param fileName
   * @return
   */
  def presentation(blpu: BLPU, lpi: LPI, street: StreetWithDescription, deliveryPoint: Option[DeliveryPoint], fileName: String) = {

    def streetMatchesAnyOfTownLocalityArea(streetName: String) =
      street.localityName.getOrElse("").equalsIgnoreCase(streetName) || street.townName.getOrElse("").equalsIgnoreCase(streetName) || street.administrativeArea.equalsIgnoreCase(streetName)


    val streetDescription = toSentenceCase(constructStreetAddressFrom(lpi, street, deliveryPoint, fileName)) match {
      case Some(s) if constructPropertyFrom(lpi).isDefined && streetMatchesAnyOfTownLocalityArea(s) => None // if we have a property and street is the same as another field strip street
      case Some(s) => Some(s) // Normal have street return it
      case _ => None // No street
    }

    // If the street matches an individal field strip that field
    val locality = streetDescription match {
      case Some(sd) if !street.localityName.getOrElse("").equalsIgnoreCase(sd) => toSentenceCase(street.localityName)
      case None => toSentenceCase(street.localityName)
      case _ => None
    }

    val town = streetDescription match {
      case Some(sd) if !street.townName.getOrElse("").equalsIgnoreCase(sd) => toSentenceCase(street.townName)
      case None => toSentenceCase(street.townName)
      case _ => None
    }

    val area = streetDescription match {
      case Some(sd) if !constructArea(street).getOrElse("").equalsIgnoreCase(sd) => constructArea(street)
      case None => constructArea(street)
      case _ => None
    }

    Presentation(
      property = toSentenceCase(constructPropertyFrom(lpi)),
      street = streetDescription,
      locality = locality,
      town = town,
      area = area,
      postcode = blpu.postcode
    )
  }

  /**
   * Compare area and town, returning something for area only if town and area are different
   * @param street
   * @return
   */
  def constructArea(street: StreetWithDescription) =
    toSentenceCase(
      if (!street.townName.isDefined) street.administrativeArea
      else if (street.townName.isDefined && street.townName.get.equals(street.administrativeArea)) None
      else street.administrativeArea
    )

  def details(addressWrapper: AddressBaseWrapper, filename: String) = Details(
    blpuCreatedAt = addressWrapper.blpu.startDate,
    blpuUpdatedAt = addressWrapper.blpu.lastUpdated,
    classification = addressWrapper.classification.classificationCode,
    status = addressWrapper.blpu.blpuState.map(pp => pp.toString),
    state = addressWrapper.blpu.logicalState.map(pp => pp.toString),
    isPostalAddress = addressWrapper.blpu.canReceivePost,
    isResidential = addressWrapper.classification.isResidential,
    isCommercial = addressWrapper.classification.isCommercial,
    isHigherEducational = addressWrapper.classification.isEducational,
    isElectoral = (addressWrapper.classification.isEducational || addressWrapper.classification.isResidential) && addressWrapper.blpu.canReceivePost,
    usrn = addressWrapper.lpi.usrn,
    file = filename,
    organisation = toSentenceCase(addressWrapper.organisation.map(org => org.organistation)),
    primaryClassification = addressWrapper.classification.primaryUse,
    secondaryClassification = addressWrapper.classification.secondaryUse
  )

}

/**
 * Various utilities for string / field conversions / creations
 */
object formatters extends Logging {

  /**
   * Takes an LPI and builds the property fields from the SAO and PAO objects as per OS recommendations
   * SAO_TEXT -> SAO_NUMBERS -> PAO_NUMBERS -> PAO_TEXT
   * Returns in sentence case
   * @param lpi
   * @return String: property
   */
  def constructPropertyFrom(lpi: LPI) = {
    val formatted = (
      toSentenceCase(lpi.saoText).toList ++
        formatStartAndEndNumbersAndSuffixes(lpi.saoStartNumber, lpi.saoStartSuffix, lpi.saoEndNumber, lpi.saoEndSuffix).toList ++
        toSentenceCase(lpi.paoText).toList
      ).mkString(" ")

    if (formatted.isEmpty) None
    else Some(formatted)
  }

  /**
   * Build street address (ie 4 High Street).
   * If Street Description is of classification unofficial or description then use delivery point fields as street name
   * Rules are:
   * If not an invalid street description or description is under 20 chars (tend to be correct) use street description as normal (standard use case)
   * Else If invalid street description and no property defined, use delivery point to construct street
   * Else no street can be built
   *
   * Give every chance to make correct street - very inconsitant data on street description (lots errounously marked as descriptions) and
   * delivery point has potentially useful data in all 4 fields.
   * So give dispensation to street - even if a street description, if it's short (<20) then use it
   * If not only use delivery point if there is no property field, if a property exists this is enought to give the user information
   * Otherwise we're bust
   * @param lpi
   * @param street
   * @param deliveryPoint
   * @return
   */
  /**
   * Sequence to make a street:
   * - standard use case - ie right type - use street description
   * - Then if no property and <20 and wrong type - use street description (balance of probabilities it's ok)
   * - Then no property then try delivery point (starts to get more difficult)
   * - No property, street or delivery point, just return street number (Last change anything is better than nothing)
   * - Then nothing - address will fail audit and not be included
   */
  def constructStreetAddressFrom(lpi: LPI, street: StreetWithDescription, deliveryPoint: Option[DeliveryPoint], file: String) = {

    def makeStreet(lpi: LPI, street: String) = Some(String.format("%s %s", constructStreetAddressPrefixFrom(lpi).getOrElse(""), toSentenceCase(street).get).trim)

    val streetOption = chooseStreetDescription(lpi, street, deliveryPoint, file)

    if (streetOption.isDefined) makeStreet(lpi, streetOption.get)
    else if (!constructPropertyFrom(lpi).isDefined) {
      // if we have a street number just return that
      logger.info("UPDATED [no property using PAO fields]: street classification [%s] uprn [%s] usrn [%s] description [%s] file [%s]".format(street.recordType, lpi.uprn, lpi.usrn, street.streetDescription, file))
      constructStreetAddressPrefixFrom(lpi)
    } else {
      logger.info("UPDATED [no street]: street classification [%s] uprn [%s] usrn [%s] description [%s] file [%s]".format(street.recordType, lpi.uprn, lpi.usrn, street.streetDescription, file))
      None
    }
  }

  def chooseStreetDescription(lpi: LPI, street: StreetWithDescription, deliveryPoint: Option[DeliveryPoint], file: String) = {

    if (!invalidStreetDescription(street)) {
      Some(street.streetDescription)
    } else {
      logger.info("UPDATED [Using delivery point for street]: street classification [%s] uprn [%s] usrn [%s], description [%s] delivery point [%s] file [%s]".format(street.recordType, lpi.uprn, lpi.usrn, street.streetDescription, deliveryPoint, file))
      deliveryPointStreet(deliveryPoint) match {
        case Some(dpStreet) => {
          if (constructPropertyFrom(lpi).getOrElse("").toLowerCase.contains(dpStreet.toLowerCase)) {
            logger.info("UPDATED [street matches property]: street classification [%s] uprn [%s] usrn [%s], description [%s] delivery point [%s] file [%s]".format(street.recordType, lpi.uprn, lpi.usrn, street.streetDescription, deliveryPoint, file))
            None
          }
          else Some(dpStreet)
        }
        case _ => None
      }
    }
  }

  /**
   * Construct street name from delivery point entry
   * Order is throughfare -> dependantthroughfare -> doubleDependantLocality
   * any one of these can be populated in the data. this is considered best order
   * Pick best one and return - None if no delivery point or no street fields populated
   * @param deliveryPoint
   * @return Option[String] street
   */

  def deliveryPointStreet(deliveryPoint: Option[DeliveryPoint]) =
    deliveryPoint match {
      case Some(dp) if dp.thoroughfareName.isDefined => logger.info("UPDATED [Using delivery point for street] uprn [%s] thoroughfare [%s]".format(dp.uprn, dp.thoroughfareName.get)); Some(dp.thoroughfareName.get)
      case Some(dp) if dp.dependantThoroughfareName.isDefined => logger.info("UPDATED [Using delivery point for street] uprn [%s] dependantThoroughfareName [%s]".format(dp.uprn, dp.dependantThoroughfareName.get)); Some(dp.dependantThoroughfareName.get)
      case Some(dp) if dp.doubleDependantLocality.isDefined => logger.info("UPDATED [Using delivery point for street] uprn [%s] doubleDependantLocality [%s]".format(dp.uprn, dp.doubleDependantLocality.get)); Some(dp.doubleDependantLocality.get)
      case _ => None
    }


  def invalidStreetDescription(street: StreetWithDescription) = !street.recordType.isDefined || StreetRecordTypeCode.isUnofficialStreet(street.recordType.get) || StreetRecordTypeCode.isDescription(street.recordType.get)

  def constructStreetAddressPrefixFrom(lpi: LPI) = formatStartAndEndNumbersAndSuffixes(lpi.paoStartNumber, lpi.paoStartSuffix, lpi.paoEndNumber, lpi.paoEndSuffix)

  /**
   * Rules on this are:
   * If BOTH start and end INCLUDE BOTH
   * If ANY suffixes INCLUDE ONLY IF PARTNERED WITH A NUMBER
   * If ONLY start include START
   * If ONLY end include END
   * If ONLY suffixes include NOTHING
   * @param startNumber
   * @param startSuffix
   * @param endNumber
   * @param endSuffix
   * @return
   */
  def formatStartAndEndNumbersAndSuffixes(startNumber: Option[String], startSuffix: Option[String], endNumber: Option[String], endSuffix: Option[String]) = {
    val start = if (startNumber.isDefined) List(startNumber, startSuffix).flatten.mkString("") else ""
    val end = if (endNumber.isDefined) List(endNumber, endSuffix).flatten.mkString("") else ""

    startNumber.isDefined && endNumber.isDefined match {
      case true => Some(start + "-" + end)
      case false if startNumber.isDefined => Some(start)
      case false if endNumber.isDefined => Some(end)
      case _ => None
    }
  }

  def toSentenceCase(field: Option[String]) = field.map(f => (f toLowerCase) split " " map (_.capitalize) mkString " ")

  def stripAllWhitespace(from: String) = from replaceAll(" ", "") trim

  def lowercase(from: String) = from toLowerCase
}