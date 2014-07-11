package uk.gov.gds.location.importer.model

import org.joda.time.DateTime
import uk.gov.gds.location.importer.model.CodeLists.{LogicalStatusCode, BlpuStateCode}

/**
 * Address for use in test queries and so on.
 * Maps to a test gss code for use in other applications - IER for example uses E99999999 as a test Local Authority
 * Therefore we can map this test query to a corresponding test ero
 */
object TestAddress {

  lazy val testAddress = {
    val p = Presentation(
      property = "property",
      street = "street",
      locality = "locality",
      town = "town",
      area = "area",
      postcode = "X1 1XX"
    )

    val d = Details(
      blpuCreatedAt = new DateTime(),
      blpuUpdatedAt = new DateTime(),
      classification = "RD",
      status = Some(BlpuStateCode.inUse.toString),
      state = Some(LogicalStatusCode.approved.toString),
      isPostalAddress = true,
      isResidential = true,
      isCommercial = false,
      isHigherEducational = false,
      isElectoral = true,
      usrn = "1234",
      file = "test.csv",
      organisation = None,
      primaryClassification = "Residential",
      secondaryClassification = Some("Dwelling")
    )

    val o = OrderingHelpers(
      saoText = Some("property")
    )

    val l = Location(1.1, 0.0)

    Address(
      postcode = "x11xx",
      gssCode = "E99999999",
      country = "England",
      uprn = "999999999999",
      createdAt = new DateTime(),
      presentation = p,
      details = d,
      location = l,
      ordering = Some(o)
    )
  }
}
