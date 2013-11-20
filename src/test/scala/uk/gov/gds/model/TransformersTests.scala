package uk.gov.gds.model

import org.specs2.mutable.Specification
import scalax.io.{CloseableIterator, DefaultResourceContext, LineTraversable}
import scalax.io.Line.Terminators.NewLine
import Transformers._
import scala.collection.mutable

class TransformersTests extends Specification {

  private val validLinesForBLPU = List(
    """21,"I",94755,9059007610,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BX",0""",
    """21,"I",94755,9059007611,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BY",0""",
    """21,"I",94755,9059007612,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
  )

  private val validLinesForLPI = List(
    """24,"I",92423,9059007610,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803241,"1","Area 51","level","Y"""",
    """24,"I",92423,9059007611,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803242,"1","Area 51","level","Y"""",
    """24,"I",92423,9059007612,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803243,"1","Area 51","level","Y""""
  )

  private val validLinesForClassification = List(
    """32,"I",94733,9059004871,"9059C000003726","C","AddressBase Premium Classification Scheme",1.0,2010-04-22,,2012-01-11,2010-04-22""",
    """32,"I",94733,9059004872,"9059C000003726","R","AddressBase Premium Classification Scheme",1.0,2010-04-23,,2012-01-12,2010-04-23""",
    """32,"I",94733,9059004873,"9059C000003726","RD04","AddressBase Premium Classification Scheme",1.0,2010-04-24,,2012-01-13,2010-04-24"""
  )
  private val validLinesForOrganisation = List(
    """31,"I",93718,9059017793,"9059O000000971","Hannahs","",2012-02-01,,2012-02-01,2012-02-01""",
    """31,"I",93835,9059085157,"9059O000001679","Party Time","",2012-03-20,,2012-03-20,2012-03-20""",
    """31,"I",93843,9059000379,"9059O000000619","Department Of Leisure And Culture","",2012-01-23,,2012-01-23,2012-01-23"""
  )
  private val validLinesForStreet = List(
    """11,"I",1147,709884,2,9053,2,2010-02-05,,8,0,2008-01-17,,2009-06-05,2008-01-17,349535.00,734956.00,350014.00,735097.00,999""",
    """11,"I",1149,709887,2,9053,2,2010-02-05,,8,0,2008-01-25,,2008-10-09,2008-01-25,349535.00,734956.00,349125.00,735569.00,999""",
    """11,"I",1151,709895,2,9053,2,2010-02-05,,8,0,2008-01-25,,2008-10-09,2008-01-25,347600.00,734728.00,347561.00,734677.00,999"""
  )
  private val validLinesForStreetDescriptor = List(
    """15,"I",1146,709838,"ACCESS FROM ZC4 TO GAGIE HOLDING","","MURROES","ANGUS","ENG"""",
    """15,"I",1148,709884,"ACCESS FROM ZU306 TO B962 AT LAWS","NEWBIGGING","MONIFIETH","ANGUS","ENG"""",
    """15,"I",1150,709887,"ZU306 FROM TRACK BETWEEN ZU306 AND B962 TO B961 AT DRUMSTURDY","","KINGENNIE","ANGUS","ENG""""
  )

  private def processStringLists(input: List[String], errors: mutable.MutableList[String] = mutable.MutableList.empty[String], fileName: String = "testing") = processRows(new LineTraversable(CloseableIterator(input.mkString("\n").toCharArray.iterator), NewLine, false, DefaultResourceContext))(errors, fileName)

  "Transformer" should {

    "parse BLPU lines into correctly typed objects with correct values" in {
      val processed = processStringLists(validLinesForBLPU)
      processed.size must beEqualTo(3)
      for (p <- processed) p.isInstanceOf[BLPU] must beTrue
      processed(0).asInstanceOf[BLPU].uprn must beEqualTo("9059007610")
      processed(1).asInstanceOf[BLPU].uprn must beEqualTo("9059007611")
      processed(2).asInstanceOf[BLPU].uprn must beEqualTo("9059007612")
    }

    "parse LPI lines into correctly typed objects with correct values" in {
      val processed = processStringLists(validLinesForLPI)
      processed.size must beEqualTo(3)
      for (p <- processed) p.isInstanceOf[LPI] must beTrue
      processed(0).asInstanceOf[LPI].uprn must beEqualTo("9059007610")
      processed(1).asInstanceOf[LPI].uprn must beEqualTo("9059007611")
      processed(2).asInstanceOf[LPI].uprn must beEqualTo("9059007612")
    }

    "parse Classification lines into correctly typed objects with correct values" in {
      val processed = processStringLists(validLinesForClassification)
      processed.size must beEqualTo(3)
      for (p <- processed) p.isInstanceOf[Classification] must beTrue
      processed(0).asInstanceOf[Classification].uprn must beEqualTo("9059004871")
      processed(1).asInstanceOf[Classification].uprn must beEqualTo("9059004872")
      processed(2).asInstanceOf[Classification].uprn must beEqualTo("9059004873")
    }

    "parse Organisation lines into correctly typed objects with correct values" in {
      val processed = processStringLists(validLinesForOrganisation)
      processed.size must beEqualTo(3)
      for (p <- processed) p.isInstanceOf[Organisation] must beTrue
      processed(0).asInstanceOf[Organisation].uprn must beEqualTo("9059017793")
      processed(1).asInstanceOf[Organisation].uprn must beEqualTo("9059085157")
      processed(2).asInstanceOf[Organisation].uprn must beEqualTo("9059000379")
    }

    "parse Street lines into correctly typed objects with correct values" in {
      val processed = processStringLists(validLinesForStreet)
      processed.size must beEqualTo(3)
      for (p <- processed) p.isInstanceOf[Street] must beTrue
      processed(0).asInstanceOf[Street].usrn must beEqualTo("709884")
      processed(1).asInstanceOf[Street].usrn must beEqualTo("709887")
      processed(2).asInstanceOf[Street].usrn must beEqualTo("709895")
    }

    "parse Street Descriptor lines into correctly typed objects with correct values" in {
      val processed = processStringLists(validLinesForStreetDescriptor)
      processed.size must beEqualTo(3)
      for (p <- processed) p.isInstanceOf[StreetDescriptor] must beTrue
      processed(0).asInstanceOf[StreetDescriptor].usrn must beEqualTo("709838")
      processed(1).asInstanceOf[StreetDescriptor].usrn must beEqualTo("709884")
      processed(2).asInstanceOf[StreetDescriptor].usrn must beEqualTo("709887")
    }

    "collect lines into lists of correctly typed objects with correct values" in {
      val processed = processStringLists(
        validLinesForBLPU ++
        validLinesForLPI ++
        validLinesForOrganisation ++
        validLinesForClassification ++
        validLinesForStreet ++
        validLinesForStreetDescriptor
      )

      processed.size must beEqualTo(18)

      val blpus = extractBlpus(processed)
      for (p <- blpus) p.isInstanceOf[BLPU] must beTrue
      blpus.size must beEqualTo(3)

      val lpis = extractLpis(processed)
      for (p <- lpis) p.isInstanceOf[LPI] must beTrue
      lpis.size must beEqualTo(3)

      val streets = extractStreets(processed)
      for (p <- streets) p._2.isInstanceOf[Street] must beTrue
      streets.size must beEqualTo(3)

      val streetDescriptors = extractStreetDescriptors(processed)
      for (p <- streetDescriptors) p._2.isInstanceOf[StreetDescriptor] must beTrue
      streetDescriptors.size must beEqualTo(3)

      val organisations = extractOrganisations(processed)
      for (p <- organisations) p.isInstanceOf[Organisation] must beTrue
      organisations.size must beEqualTo(3)

      val classifications = extractClassifications(processed)
      for (p <- classifications) p.isInstanceOf[Classification] must beTrue
      classifications.size must beEqualTo(3)
    }

    "construct an address base wrapper object with the correct BLPUs" in {
      val processed = processStringLists(validLinesForBLPU ++ validLinesForLPI)
      val addressWrappers = constructAddressBaseWrapper(extractBlpus(processed), List.empty[LPI])

      addressWrappers.size must beEqualTo(3)
      addressWrappers(0).blpu.postcode must beEqualTo("DD5 3BX")
      addressWrappers(1).blpu.postcode must beEqualTo("DD5 3BY")
      addressWrappers(2).blpu.postcode must beEqualTo("DD5 3BZ")

    }

    "construct an address base wrapper object with the LPI associated with correct BLPU" in {
      val processed = processStringLists(validLinesForBLPU ++ validLinesForLPI)
      val addressWrappers = constructAddressBaseWrapper(extractBlpus(processed), extractLpis(processed))

      addressWrappers.size must beEqualTo(3)

      addressWrappers(0).blpu.postcode must beEqualTo("DD5 3BX")
      addressWrappers(0).lpis.size must beEqualTo(1)
      addressWrappers(0).lpis(0).usrn must beEqualTo("7803241")


      addressWrappers(1).blpu.postcode must beEqualTo("DD5 3BY")
      addressWrappers(1).lpis.size must beEqualTo(1)
      addressWrappers(1).lpis(0).usrn must beEqualTo("7803242")

      addressWrappers(2).blpu.postcode must beEqualTo("DD5 3BZ")
      addressWrappers(2).lpis.size must beEqualTo(1)
      addressWrappers(2).lpis(0).usrn must beEqualTo("7803243")

    }
  }

}
