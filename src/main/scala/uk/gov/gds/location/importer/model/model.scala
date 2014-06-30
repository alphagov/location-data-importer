package uk.gov.gds.location.importer.model

import org.joda.time.DateTime
import uk.gov.gds.location.importer.model.CodeLists._
import com.novus.salat._
import com.novus.salat.global._
import uk.gov.gds.location.importer.model.ClassificationCodes._
import uk.gov.gds.location.importer.model.CodeLists.StreetSurfaceCode.StreetSurfaceCode
import uk.gov.gds.location.importer.model.CodeLists.StreetClassificationCode.StreetClassificationCode
import uk.gov.gds.location.importer.model.CodeLists.StreetRecordTypeCode.StreetRecordTypeCode
import uk.gov.gds.location.importer.model.CodeLists.StreetRecordTypeCode
import uk.gov.gds.location.importer.model.CodeLists.LogicalStatusCode.LogicalStatusCode
import uk.gov.gds.location.importer.model.CodeLists.LogicalStatusCode
import uk.gov.gds.location.importer.model.CodeLists.BlpuStateCode.BlpuStateCode
import uk.gov.gds.location.importer.model.CodeLists.BlpuStateCode
import uk.gov.gds.location.importer.model.CodeLists.StreetStateCode
import uk.gov.gds.location.importer.model.CodeLists.StreetStateCode.StreetStateCode
import uk.gov.gds.location.importer.conversions.EastingNorthingToLatLongConvertor.gridReferenceToLatLong
import uk.gov.gds.location.importer.logging.Logging
import Countries.countries

import LocalAuthorities._

/**
 * Keeps all code point objects in memory as an optimisation
 */
object AllTheCodePoints {

  import scala.collection.mutable.{Map => MutableMap}

  var codePoints = MutableMap.empty[String, (String, String)]

  def add(codePointsToAdd: List[CodePoint]) {
    codePointsToAdd.map(c => codePoints.put(c.postcode, (c.gssCode, c.country)))
  }
}

/**
 * Keeps all street objects in memory as an optimisation
 */
object AllTheStreets {

  import scala.collection.mutable.{Map => MutableMap}

  var allTheStreets = MutableMap.empty[String, StreetWithDescription]

  def add(streets: List[StreetWithDescription]) {
    streets.map(s => allTheStreets.put(s.usrn, s))
  }
}


/**
 * Wrapper around the address base classes to associate a BLPU with dependant objects prior to translation to simple model
 */
case class AddressBaseWrapper(blpu: BLPU, lpi: LPI, classification: Classification, organisation: Option[Organisation], deliveryPoint: Option[DeliveryPoint]) {
  lazy val uprn = blpu.uprn
  lazy val usrn = lpi.usrn
}

/*
  These are the models to capture raw address base data
 */

trait AddressBase

trait AddressBaseHelpers[T <: AddressBase] {
  val recordIdentifier: String
  val requiredCsvColumns: Int
  val mandatoryCsvColumns: List[Int]

  def isMissingAMandatoryField(csvLine: List[String]) = mandatoryCsvColumns.map(column => csvLine(column).isEmpty).contains(true)

  def isValidCsvLine(csvLine: List[String]) = csvLine(0) == recordIdentifier && csvLine.size == requiredCsvColumns && !isMissingAMandatoryField(csvLine)

  def fromCsvLine(csvLine: List[String]): T

}


/**
 * Code point model
 */
case class CodePoint(postcode: String, country: String, gssCode: String, name: String) extends AddressBase {
  def serialize = grater[CodePoint].asDBObject(this)
}

object CodePoint extends AddressBaseHelpers[CodePoint] with Logging {
  private val postcodeIndex = 0
  private val countryIndex = 12
  private val gssCodeIndex = 16

  def fromCsvLine(csvLine: List[String]) = {
    CodePoint(
      csvLine(postcodeIndex).toLowerCase.replaceAll(" ", ""),
      countries(csvLine(countryIndex)),
      csvLine(gssCodeIndex),
      localAuthoritiesByGssCode(csvLine(gssCodeIndex)).onsName
    )
  }

  override def isValidCsvLine(csvLine: List[String]) = {

    if (!localAuthoritiesByGssCode.get(csvLine(gssCodeIndex)).isDefined) {
      logger.error(
        String.format(
          "Invalid codepoint row - no matching LA: gssCode [%s]", csvLine(gssCodeIndex))
      )
      false
    }
    else if (!countries.get(csvLine(countryIndex)).isDefined) {
      logger.error(
        String.format(
          "Invalid codepoint row - no matching country: country code [%s]", csvLine(countryIndex))
      )
      false
    }
    else if (csvLine.size != requiredCsvColumns) {
      logger.error(
        String.format(
          "Invalid codepoint row length: required [%s] got[%s] row details[%s]", requiredCsvColumns.toString, csvLine.size.toString, csvLine.mkString("|"))
      )
      false
    }
    else if (isMissingAMandatoryField(csvLine)) {
      logger.error(
        String.format(
          "Invalid codepoint row missing mandatory fields: required columns [%s] row details[%s]", mandatoryCsvColumns, csvLine.mkString("|"))
      )
      false
    }
    else
      true
  }

  val recordIdentifier = ""
  // not relevant for these rows
  val requiredCsvColumns = 19
  val mandatoryCsvColumns = List(postcodeIndex, countryIndex, gssCodeIndex)
}

/* Basic Land and Property Unit */
case class BLPU(
                 uprn: String,
                 blpuState: Option[BlpuStateCode],
                 logicalState: Option[LogicalStatusCode],
                 lat: Double,
                 long: Double,
                 localCustodianCode: String,
                 startDate: DateTime,
                 endDate: Option[DateTime],
                 lastUpdated: DateTime,
                 receivesPost: String,
                 postcode: String
                 ) extends AddressBase {

  def canReceivePost = !receivesPost.equals("N")
}

object BLPU extends AddressBaseHelpers[BLPU] {


  val recordIdentifier = "21"
  val requiredCsvColumns = 19

  private val uprnIndex = 3
  private val logicalStateIndex = 4
  private val blpuStateIndex = 5
  private val eastingIndex = 8
  private val northingIndex = 9
  private val localCustodianCodeIndex = 11
  private val startDateIndex = 12
  private val endDateIndex = 13
  private val updatedDateIndex = 14
  private val receivesPostIndex = 16
  private val postcodeIndex = 17

  def fromCsvLine(csvLine: List[String]) = {

    val latLong = gridReferenceToLatLong(csvLine(eastingIndex), csvLine(northingIndex))

    BLPU(
      csvLine(uprnIndex),
      BlpuStateCode.forId(csvLine(blpuStateIndex)),
      LogicalStatusCode.forId(csvLine(logicalStateIndex)),
      latLong.lat,
      latLong.long,
      csvLine(localCustodianCodeIndex),
      csvLine(startDateIndex),
      csvLine(endDateIndex),
      csvLine(updatedDateIndex),
      csvLine(receivesPostIndex),
      csvLine(postcodeIndex)
    )
  }

  val mandatoryCsvColumns = List(uprnIndex, logicalStateIndex, eastingIndex, northingIndex, localCustodianCodeIndex, startDateIndex, updatedDateIndex, postcodeIndex)
}

/* Land and Property Identitifier */
case class LPI(
                uprn: String,
                usrn: String,
                logicalState: Option[LogicalStatusCode],
                startDate: DateTime,
                endDate: Option[DateTime],
                lastUpdated: DateTime,
                paoStartNumber: Option[String],
                paoStartSuffix: Option[String],
                paoEndNumber: Option[String],
                paoEndSuffix: Option[String],
                paoText: Option[String],
                saoStartNumber: Option[String],
                saoStartSuffix: Option[String],
                saoEndNumber: Option[String],
                saoEndSuffix: Option[String],
                saoText: Option[String],
                areaName: Option[String],
                officialAddress: Option[Boolean],
                language: String
                ) extends AddressBase

object LPI extends AddressBaseHelpers[LPI] {
  val recordIdentifier = "24"
  val requiredCsvColumns = 26

  private val uprnIndex = 3
  private val languageIndex = 5
  private val logicalStateIndex = 6
  private val startDateIndex = 7
  private val endDateIndex = 8
  private val updatedDateIndex = 9
  private val saoStartNumber = 11
  private val saoStartSuffix = 12
  private val saoEndNumber = 13
  private val saoEndSuffix = 14
  private val saoText = 15
  private val paoStartNumber = 16
  private val paoStartSuffix = 17
  private val paoEndNumber = 18
  private val paoEndSuffix = 19
  private val paoText = 20
  private val usrnIndex = 21
  private val areaNameIndex = 23
  private val officialFlagIndex = 25

  override def isValidCsvLine(csvLine: List[String]) = super.isValidCsvLine(csvLine) && (!csvLine(paoText).isEmpty || !csvLine(paoStartNumber).isEmpty)

  def fromCsvLine(csvLine: List[String]) = LPI(
    csvLine(uprnIndex),
    csvLine(usrnIndex),
    LogicalStatusCode.forId(csvLine(logicalStateIndex)),
    csvLine(startDateIndex),
    csvLine(endDateIndex),
    csvLine(updatedDateIndex),
    csvLine(paoStartNumber),
    csvLine(paoStartSuffix),
    csvLine(paoEndNumber),
    csvLine(paoEndSuffix),
    csvLine(paoText),
    csvLine(saoStartNumber),
    csvLine(saoStartSuffix),
    csvLine(saoEndNumber),
    csvLine(saoEndSuffix),
    csvLine(saoText),
    csvLine(areaNameIndex),
    csvLine(officialFlagIndex),
    csvLine(languageIndex)
  )

  val mandatoryCsvColumns = List(uprnIndex, usrnIndex, languageIndex, logicalStateIndex, startDateIndex, updatedDateIndex)

}

case class Street(usrn: String,
                  recordType: Option[StreetRecordTypeCode],
                  state: Option[StreetStateCode],
                  surface: Option[StreetSurfaceCode],
                  classification: Option[StreetClassificationCode],
                  startDate: DateTime,
                  endDate: Option[DateTime],
                  lastUpdated: DateTime
                   ) extends AddressBase

object Street extends AddressBaseHelpers[Street] {
  val recordIdentifier = "11"
  val requiredCsvColumns = 20

  private val usrnIndex = 3
  private val recordTypeIndex = 4
  private val stateIndex = 6
  private val surfaceIndex = 8
  private val classificationIndex = 9
  private val startDateIndex = 11
  private val endDateIndex = 12
  private val updatedDateIndex = 13

  def fromCsvLine(csvLine: List[String]) = Street(
    csvLine(usrnIndex),
    StreetRecordTypeCode.forId(csvLine(recordTypeIndex)),
    StreetStateCode.forId(csvLine(stateIndex)),
    StreetSurfaceCode.forId(csvLine(surfaceIndex)),
    StreetClassificationCode.forId(csvLine(classificationIndex)),
    csvLine(startDateIndex),
    csvLine(endDateIndex),
    csvLine(updatedDateIndex)
  )

  val mandatoryCsvColumns = List(usrnIndex, recordTypeIndex, startDateIndex, updatedDateIndex)
}

case class StreetDescriptor(
                             usrn: String,
                             streetDescription: String,
                             localityName: Option[String],
                             townName: Option[String],
                             administrativeArea: String,
                             language: String
                             ) extends AddressBase {

  def serialize = grater[StreetDescriptor].asDBObject(this)
}

object StreetDescriptor extends AddressBaseHelpers[StreetDescriptor] {
  val recordIdentifier = "15"
  val requiredCsvColumns = 9


  private val usrnIndex = 3
  private val streetDescriptionIndex = 4
  private val localityNameIndex = 5
  private val townNameIndex = 6
  private val administrativeAreaIndex = 7
  private val languageIndex = 8


  def fromCsvLine(csvLine: List[String]) = StreetDescriptor(
    csvLine(usrnIndex),
    csvLine(streetDescriptionIndex),
    csvLine(localityNameIndex),
    csvLine(townNameIndex),
    csvLine(administrativeAreaIndex),
    csvLine(languageIndex)
  )

  val mandatoryCsvColumns = List(usrnIndex, streetDescriptionIndex, languageIndex, administrativeAreaIndex)
}

case class Organisation(
                         uprn: String,
                         organistation: String,
                         startDate: DateTime,
                         endDate: Option[DateTime],
                         lastUpdated: DateTime
                         ) extends AddressBase

object Organisation extends AddressBaseHelpers[Organisation] {
  val recordIdentifier = "31"
  val requiredCsvColumns = 11

  private val uprnIndex = 3
  private val organisationIndex = 5
  private val startDateIndex = 7
  private val endDateIndex = 8
  private val updatedDateIndex = 9

  def fromCsvLine(csvLine: List[String]) = Organisation(
    csvLine(uprnIndex),
    csvLine(organisationIndex),
    csvLine(startDateIndex),
    csvLine(endDateIndex),
    csvLine(updatedDateIndex)
  )

  val mandatoryCsvColumns = List(uprnIndex, organisationIndex, startDateIndex, updatedDateIndex)
}

case class DeliveryPoint(
                          uprn: String,
                          subBuildingName: Option[String],
                          buildingName: Option[String],
                          buildingNumber: Option[String],
                          dependantThoroughfareName: Option[String],
                          thoroughfareName: Option[String],
                          doubleDependantLocality: Option[String],
                          dependantLocality: Option[String],
                          postcode: String,
                          startDate: DateTime,
                          endDate: Option[DateTime],
                          lastUpdated: DateTime) extends AddressBase

object DeliveryPoint extends AddressBaseHelpers[DeliveryPoint] {
  val recordIdentifier = "28"
  val requiredCsvColumns = 29

  private val uprnIndex = 3
  private val subBuildingNameIndex = 8
  private val buildingNameIndex = 9
  private val buildingNumberIndex = 10
  private val dependantThoroughfareNameIndex = 11
  private val thoroughfareNameIndex = 12
  private val doubleDependantLocality = 13
  private val dependantLocality = 14
  private val postcodeIndex = 16
  private val startDateIndex = 25
  private val endDateIndex = 26
  private val updatedDateIndex = 27

  val mandatoryCsvColumns = List(uprnIndex, postcodeIndex, startDateIndex, updatedDateIndex)

  def fromCsvLine(csvLine: List[String]) = DeliveryPoint(
    csvLine(uprnIndex),
    csvLine(subBuildingNameIndex),
    csvLine(buildingNameIndex),
    csvLine(buildingNumberIndex),
    csvLine(dependantThoroughfareNameIndex),
    csvLine(thoroughfareNameIndex),
    csvLine(doubleDependantLocality),
    csvLine(dependantLocality),
    csvLine(postcodeIndex),
    csvLine(startDateIndex),
    csvLine(endDateIndex),
    csvLine(updatedDateIndex)
  )
}

case class Classification(
                           uprn: String,
                           classificationCode: String,
                           startDate: DateTime,
                           endDate: Option[DateTime],
                           lastUpdated: DateTime,
                           primaryUse: String,
                           secondaryUse: Option[String]
                           ) extends AddressBase {
  def isResidential = ClassificationCodes.isResidential(classificationCode)

  def isCommercial = ClassificationCodes.isCommercial(classificationCode)

  def isEducational = ClassificationCodes.isHigherEducational(classificationCode)
}

object Classification extends AddressBaseHelpers[Classification] {
  val recordIdentifier = "32"
  val requiredCsvColumns = 12

  private val uprnIndex = 3
  private val classificationCodeIndex = 5
  private val startDateIndex = 8
  private val endDateIndex = 9
  private val updatedDateIndex = 10

  def fromCsvLine(csvLine: List[String]) = Classification(
    csvLine(uprnIndex),
    csvLine(classificationCodeIndex),
    csvLine(startDateIndex),
    csvLine(endDateIndex),
    csvLine(updatedDateIndex),
    primaryCodeFor(csvLine(classificationCodeIndex)).get,
    secondaryCodeFor(csvLine(classificationCodeIndex))
  )


  /**
   * Must have a valid, mapped primary classification to be valid
   * @param csvLine
   * @return
   */
  override def isValidCsvLine(csvLine: List[String]) = {
    super.isValidCsvLine(csvLine) && primaryCodeFor(csvLine(classificationCodeIndex)).isDefined
  }


  val mandatoryCsvColumns = List(uprnIndex, classificationCodeIndex, startDateIndex, updatedDateIndex)
}

/*
  These case classes are the model we translate too and persist
 */
case class Location(lat: Double, long: Double)

case class Details(
                    blpuCreatedAt: DateTime,
                    blpuUpdatedAt: DateTime,
                    classification: String,
                    status: Option[String] = None,
                    state: Option[String] = None,
                    isPostalAddress: Boolean,
                    isCommercial: Boolean,
                    isResidential: Boolean,
                    isHigherEducational: Boolean,
                    isElectoral: Boolean,
                    usrn: String,
                    file: String,
                    organisation: Option[String],
                    primaryClassification: String,
                    secondaryClassification: Option[String]
                    )

case class Presentation(
                         property: Option[String] = None,
                         street: Option[String],
                         locality: Option[String] = None,
                         town: Option[String] = None,
                         area: Option[String] = None,
                         postcode: String
                         )

case class OrderingHelpers(
                            saoStartNumber: Option[Int] = None,
                            saoStartSuffix: Option[String] = None,
                            saoEndNumber: Option[Int] = None,
                            saoEndSuffix: Option[String] = None,
                            paoStartNumber: Option[Int] = None,
                            paoStartSuffix: Option[String] = None,
                            paoEndNumber: Option[Int] = None,
                            paoEndSuffix: Option[String] = None,
                            paoText: Option[String] = None,
                            saoText: Option[String] = None
                            )

case class Address(
                    postcode: String,
                    gssCode: String,
                    country: String,
                    uprn: String,
                    createdAt: DateTime = new DateTime,
                    presentation: Presentation,
                    location: Location,
                    details: Details,
                    ordering: Option[OrderingHelpers] = None
                    ) {
  def serialize = grater[Address].asDBObject(this)

}

case class StreetWithDescription(
                                  usrn: String,
                                  streetDescription: String,
                                  localityName: Option[String],
                                  townName: Option[String],
                                  administrativeArea: String,
                                  recordType: Option[String],
                                  state: Option[String],
                                  surface: Option[String],
                                  classification: Option[String],
                                  file: String
                                  ) {
  def serialize = grater[StreetWithDescription].asDBObject(this)
}

/**
 * Boundary line model
 */
case class Properties(NAME: String, CODE: String)

case class AuthorityBoundary(
                              properties: Properties
                              ) {
  def serialize = grater[AuthorityBoundary].asDBObject(this)
}

