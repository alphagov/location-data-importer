package uk.gov.gds.location.importer.postgres

import scala.slick.driver.PostgresDriver.simple._
import uk.gov.gds.location.importer.model.Address
import java.sql.Timestamp


object PostgresConnection {

  val addresses = TableQuery[Addresses]
  val addressDetails = TableQuery[AddressDetails]
  val ordering = TableQuery[Ordering]

  def create() {
    Database.forURL("jdbc:postgresql://localhost/locate", driver = "org.postgresql.Driver") withSession {
      implicit session =>

        addresses.ddl.create
        addressDetails.ddl.create
        ordering.ddl.create
    }
  }

  def insertAddresses(things: List[Address]) {
    Database.forURL("jdbc:postgresql://localhost/locate", driver = "org.postgresql.Driver") withSession {
      implicit session =>

        addresses ++= things.map(
          address => (
            address.uprn,
            address.postcode,
            address.country,
            address.gssCode,
            new Timestamp(address.createdAt.getMillis),
            address.presentation.property,
            address.presentation.street,
            address.presentation.locality,
            address.presentation.town,
            address.presentation.area,
            address.presentation.postcode,
            address.location.lat,
            address.location.long
            )
        )

        ordering ++= things.map(
          address => (
            address.uprn,
            address.ordering.get.saoStartNumber,
            address.ordering.get.saoStartSuffix,
            address.ordering.get.saoEndNumber,
            address.ordering.get.saoEndSuffix,
            address.ordering.get.paoStartNumber,
            address.ordering.get.paoStartSuffix,
            address.ordering.get.paoEndNumber,
            address.ordering.get.paoEndSuffix,
            address.ordering.get.paoText,
            address.ordering.get.saoText
            )
        )

        addressDetails ++= things.map(
          address => (
            address.uprn,
            new Timestamp(address.details.blpuCreatedAt.getMillis),
            new Timestamp(address.details.blpuUpdatedAt.getMillis),
            address.details.classification,
            address.details.status,
            address.details.state,
            address.details.isPostalAddress,
            address.details.isCommercial,
            address.details.isResidential,
            address.details.isHigherEducational,
            address.details.isElectoral,
            address.details.usrn,
            address.details.file,
            address.details.organisation,
            address.details.primaryClassification,
            address.details.secondaryClassification
            )
        )
    }
  }


  class Addresses(tag: Tag) extends Table[(
    String, String, String, String, Timestamp, Option[String], Option[String], Option[String], Option[String], Option[String], String, Double, Double)](tag, "addresses") {
    def uprn = column[String]("uprn", O.PrimaryKey)

    def searchPostcode = column[String]("search_postcode")

    def country = column[String]("country")

    def gssCode = column[String]("gss_code")

    def createdAt = column[Timestamp]("created_at")

    def property = column[Option[String]]("property")

    def street = column[Option[String]]("street")

    def locality = column[Option[String]]("locality")

    def town = column[Option[String]]("town")

    def area = column[Option[String]]("area")

    def postcode = column[String]("postcode")

    def lat = column[Double]("lat")

    def long = column[Double]("long")

    def postcode_idx = index("idx_postcode", (searchPostcode), unique = false)

    def gss_code_idx = index("idx_gss_code", (gssCode), unique = false)

    def * = (uprn, searchPostcode, country, gssCode, createdAt, property, street, locality, town, area, postcode, lat, long)
  }

  class Ordering(tag: Tag) extends Table[(String, Option[Int], Option[String], Option[Int], Option[String], Option[Int], Option[String], Option[Int], Option[String], Option[String], Option[String])](tag, "ordering") {
    def uprn = column[String]("uprn", O.PrimaryKey)

    def saoStartNumber = column[Option[Int]]("sao_start_number")

    def saoStartSuffix = column[Option[String]]("sao_start_suffix")

    def saoEndNumber = column[Option[Int]]("sao_end_number")

    def saoEndSuffix = column[Option[String]]("sao_end_suffix")

    def paoStartNumber = column[Option[Int]]("pao_start_number")

    def paoStartSuffix = column[Option[String]]("pao_start_suffix")

    def paoEndNumber = column[Option[Int]]("pao_end_number")

    def paoEndSuffix = column[Option[String]]("pao_end_suffix")

    def paoText = column[Option[String]]("pao_text")

    def saoText = column[Option[String]]("sao_text")

    def addressFk = foreignKey("address_uprn_fk", uprn, addresses)(_.uprn)

    def * = (uprn, saoStartNumber, saoStartSuffix, saoEndNumber, saoEndSuffix, paoStartNumber, paoStartSuffix, paoEndNumber, paoEndSuffix, paoText, saoText)
  }


  class AddressDetails(tag: Tag) extends Table[(String,
    Timestamp, Timestamp, String, Option[String], Option[String], Boolean, Boolean, Boolean, Boolean, Boolean, String, String, Option[String], String, Option[String]
    )](tag, "address_details") {
    def uprn = column[String]("uprn")

    def blpuCreatedAt = column[Timestamp]("blpu_created_at")

    def blpuUpdatedAt = column[Timestamp]("blpu_updated_at")

    def classification = column[String]("classification")

    def status = column[Option[String]]("status")

    def state = column[Option[String]]("state")

    def isPostalAddress = column[Boolean]("is_postal_address")

    def isCommercial = column[Boolean]("is_commercial")

    def isResidential = column[Boolean]("is_residential")

    def isHigherEducational = column[Boolean]("is_higher_educational")

    def isElectoral = column[Boolean]("is_electoral")

    def usrn = column[String]("usrn")

    def file = column[String]("file")

    def organisation = column[Option[String]]("organisation")

    def primaryClassification = column[String]("primary_classification")

    def secondaryClassification = column[Option[String]]("secondary_classification")

    def addressFk = foreignKey("address_uprn_fk", uprn, addresses)(_.uprn)

    def * = (uprn, blpuCreatedAt, blpuUpdatedAt, classification, status, state, isPostalAddress, isCommercial, isResidential, isHigherEducational, isElectoral, usrn, file, organisation, primaryClassification, secondaryClassification)
  }

}
