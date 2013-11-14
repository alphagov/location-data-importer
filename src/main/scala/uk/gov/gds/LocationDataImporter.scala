package uk.gov.gds

import scopt.OptionParser
import uk.gov.gds.io.FileLoader

object LocationDataImporter {

  case class Config(dir: Option[String] = None)

  def main(args: Array[String]) {

    val opts = new OptionParser[Config]("Location Data Importer") {
        head("Parse and import location data", "0.1")
        opt[String]('d', "dir") text ( "Location of address base files files") required() action {
          (dir: String, c: Config) => c.copy(dir = Some(dir))
        }
        help("help") text("use -d or -dir to identify source directory containg files to parse")
        version("version") text("0.1")
    }

    opts.parse(args, Config()) map {
      config => {
        if(config.dir.isDefined) {
          FileLoader.loadFile(config.dir.get)
        }
      }
    }
	}

}
