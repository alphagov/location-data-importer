package uk.gov.gds


import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.conversions.scala._
import uk.gov.gds.logging.Logging

class MongoConnection(username: Option[String] = None, password: Option[String] = None) extends Logging {
  RegisterJodaTimeConversionHelpers()

  private val mongoClient = MongoClient()

  private val db = mongoClient("location")

  private val collection = db.getCollection("address")

  private def authenticate() {
    if(username.isDefined && password.isDefined) db.authenticate(username.get, password.get)
  }

  authenticate()

  def insert(things: List[DBObject]) = collection.insert(things.toArray, WriteConcern.Normal)

  def addIndexes() {
    logger.info("indexing lcpostcode")
    db.getCollection("address").ensureIndex(DBObject("lcPostcode" -> 1))
    logger.info("indexing uprn")
    db.getCollection("address").ensureIndex(DBObject("uprn" -> 1))
  }
}
