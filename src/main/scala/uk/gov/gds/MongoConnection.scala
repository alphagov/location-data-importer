package uk.gov.gds


import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.conversions.scala._

class MongoConnection {
  RegisterJodaTimeConversionHelpers()

  private val mongoClient = MongoClient()

  private val db = mongoClient("location")

  private val collection = db.getCollection("address")

  def insert = {
    collection.insert(MongoDBObject("key" -> "value"))
  }

  def insert(things: List[DBObject]) = collection.insert(things.toArray, WriteConcern.Normal)
}
