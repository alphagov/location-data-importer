package uk.gov.gds.location.importer.mongo

import com.mongodb.casbah.Imports._
import uk.gov.gds.location.importer.logging.Logging
import com.novus.salat._
import com.novus.salat.global._
import uk.gov.gds.location.importer.model.CodePoint
import uk.gov.gds.location.importer.model.BoundaryLine
import scala.Some
import uk.gov.gds.location.importer.model.StreetWithDescription

class MongoConnection extends Logging {

  private val mongoClient = MongoClient()

  private val db = mongoClient("locate")

  private val addresses = db.getCollection("address")
  private val streets = db.getCollection("streets")
  private val codePoints = db.getCollection("codePoints")
  private val boundaryLine = db.getCollection("boundaryline")

  def authenticate(username: String, password: String) {
    db.authenticate(username, password)
  }

  def insertAddresses(things: List[DBObject]) = addresses.insert(things.toArray, WriteConcern.Normal)

  def insertStreets(things: List[DBObject]) = streets.insert(things.toArray, WriteConcern.Normal)

  def insertCodePoints(things: List[DBObject]) = codePoints.insert(things.toArray, WriteConcern.Normal)

  def boundaryLineForGssCode(gssCode: String, x: Double, y: Double) = {

    val point = MongoDBObject("$geometry" -> MongoDBObject("type" -> "Point", "coordinates" -> GeoCoords(x, y)))

    val b = boundaryLine.findOne(DBObject("properties.CODE" -> gssCode, "geometry" -> MongoDBObject("$geoIntersects" -> point)))

    if (b == null) {
      None
    } else {
      Some(grater[BoundaryLine].asObject(b))
    }
  }


  def boundaryLineForLatLong(x: Double, y: Double) = {

    val point = MongoDBObject("$geometry" -> MongoDBObject("type" -> "Point", "coordinates" -> GeoCoords(x, y)))

    val b = boundaryLine.findOne(DBObject("geometry" -> MongoDBObject("$geoIntersects" -> point)))

    if (b == null) {
      None
    } else {
      Some(grater[BoundaryLine].asObject(b))
    }
  }

  def streetForUsrn(usrn: String) = {
    val a = streets.findOne(DBObject("usrn" -> usrn))
    if (a == null) {
      None
    } else {
      Some(grater[StreetWithDescription].asObject(a))
    }
  }

  def codePointForPostcode(postcode: String) = {
    val a = codePoints.findOne(DBObject("postcode" -> postcode.toLowerCase.replaceAll(" ", "")))
    if (a == null) {
      None
    } else {
      Some(grater[CodePoint].asObject(a))
    }
  }

  def addAddressIndexes() {
    logger.info("indexing geo lookups - needs mongo 2.6")
    //db.getCollection("boundaryline").ensureIndex(DBObject("properties.CODE" -> 1, "geometry.coordinates" -> "2dsphere"))
    logger.info("indexing postcode")
    db.getCollection("address").ensureIndex(DBObject("postcode" -> 1))
    db.getCollection("address").ensureIndex(DBObject("houseName" -> 1, "houseNumber" -> 1))
    logger.info("indexing postcode, postal address, residential")
    db.getCollection("address").ensureIndex(DBObject("postcode" -> 1, "details.isPostalAddress" -> 1, "details.isResidential" -> 1))
    logger.info("indexing uprn")
    db.getCollection("address").ensureIndex(DBObject("uprn" -> 1))
    logger.info("indexing boundaryline")
    db.getCollection("boundaryline").ensureIndex(DBObject("properties.CODE" -> 1))
  }

  def addStreetIndexes() {
    logger.info("indexing usrn on street")
    streets.ensureIndex(DBObject("usrn" -> 1))
  }

  def addCodePointIndexes() {
    logger.info("indexing postcode on codepoint")
    codePoints.ensureIndex(DBObject("postcode" -> 1))
  }
}
