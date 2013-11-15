package uk.gov.gds.model

import org.specs2.mutable.Specification
import scalax.io.{CloseableIterator, DefaultResourceContext, LineTraversable, LongTraversable}
import scalax.io.Line.Terminators.NewLine
import Transformers._

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

  private def processStringLists(input: List[String]) = processRows(new LineTraversable(CloseableIterator(input.mkString("\n").toCharArray.iterator), NewLine, false, DefaultResourceContext))

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

    "collect lines into lists of correctly typed objects with correct values" in {
      val processed = processStringLists(validLinesForBLPU ++ validLinesForLPI)
      processed.size must beEqualTo(6)

      val blpus = extractBlpu(processed)
      for (p <- blpus) p.isInstanceOf[BLPU] must beTrue
      blpus.size must beEqualTo(3)

      val lpis = extractLpi(processed)
      for (p <- lpis) p.isInstanceOf[LPI] must beTrue
      lpis.size must beEqualTo(3)
    }

    "construct an address base wrapper object with the correct BLPUs" in {
      val processed = processStringLists(validLinesForBLPU ++ validLinesForLPI)
      val addressWrappers = constructAddressBaseWrapper(extractBlpu(processed), List.empty[LPI])

      addressWrappers.size must beEqualTo(3)
      addressWrappers("9059007610").blpu.postcode must beEqualTo("DD5 3BX")
      addressWrappers("9059007611").blpu.postcode must beEqualTo("DD5 3BY")
      addressWrappers("9059007612").blpu.postcode must beEqualTo("DD5 3BZ")

    }

    "construct an address base wrapper object with the LPI associated with correct BLPU" in {
      val processed = processStringLists(validLinesForBLPU ++ validLinesForLPI)
      val addressWrappers = constructAddressBaseWrapper(extractBlpu(processed), extractLpi(processed))

      addressWrappers.size must beEqualTo(3)

      addressWrappers("9059007610").blpu.postcode must beEqualTo("DD5 3BX")
      addressWrappers("9059007610").lpis.size must beEqualTo(1)
      addressWrappers("9059007610").lpis(0).usrn must beEqualTo("7803241")


      addressWrappers("9059007611").blpu.postcode must beEqualTo("DD5 3BY")
      addressWrappers("9059007611").lpis.size must beEqualTo(1)
      addressWrappers("9059007611").lpis(0).usrn must beEqualTo("7803242")

      addressWrappers("9059007612").blpu.postcode must beEqualTo("DD5 3BZ")
      addressWrappers("9059007612").lpis.size must beEqualTo(1)
      addressWrappers("9059007612").lpis(0).usrn must beEqualTo("7803243")

    }
  }

}
