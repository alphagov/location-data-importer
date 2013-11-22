package uk.gov.gds


import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.conversions.scala._

class MongoConnection(username: Option[String] = None, password: Option[String] = None) {
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
    db.getCollection("address").ensureIndex(DBObject("lcPostcode" -> 1))
    db.getCollection("address").ensureIndex(DBObject("property" -> 1, "streetAddress" -> 1, "town" -> 1, "area" -> 1, "postcode" -> 1, "lcPostcode" -> 1))
  }
}
