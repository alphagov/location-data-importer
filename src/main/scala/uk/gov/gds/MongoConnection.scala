package uk.gov.gds


import com.mongodb.casbah.Imports._
import uk.gov.gds.logging.Logging
import com.novus.salat._
import com.novus.salat.global._
import uk.gov.gds.model.{StreetWithDescription, StreetDescriptor}

class MongoConnection(username: Option[String] = None, password: Option[String] = None) extends Logging {

  private val mongoClient = MongoClient()

  private val db = mongoClient("location")

  private val addresses = db.getCollection("address")
  private val streets = db.getCollection("streets")

  private def authenticate() {
    if (username.isDefined && password.isDefined) db.authenticate(username.get, password.get)
  }

  authenticate()

  def insert(things: List[DBObject]) = addresses.insert(things.toArray, WriteConcern.Normal)

  def insertStreets(things: List[DBObject]) = streets.insert(things.toArray, WriteConcern.Normal)

  def streetForUsrn(usrn: String) = {
    val a = streets.findOne(DBObject("usrn" -> usrn))
    if (a == null) {
      None
    } else {
      Some(grater[StreetWithDescription].asObject(a))
    }
  }

  def addIndexes() {
    logger.info("indexing postcode")
    db.getCollection("address").ensureIndex(DBObject("postcode" -> 1))
    logger.info("indexing uprn")
    db.getCollection("address").ensureIndex(DBObject("uprn" -> 1))
  }

  def addStreetIndexes() {
    logger.info("indexing usrn on street")
    streets.ensureIndex(DBObject("usrn" -> 1))
  }
}
