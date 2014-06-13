package uk.gov.gds.location.importer.mongo

import com.mongodb.casbah.Imports._
import uk.gov.gds.location.importer.logging.Logging
import com.novus.salat._
import com.novus.salat.global._
import uk.gov.gds.location.importer.model.CodePoint
import uk.gov.gds.location.importer.model.AuthorityBoundary
import scala.Some
import uk.gov.gds.location.importer.model.StreetWithDescription

class MongoConnection extends Logging {

  private val mongoClient = MongoClient()

  private val db = mongoClient("locate-andover")

  private val addresses = db.getCollection("addresses")
  private val streets = db.getCollection("streets")
  private val postcodeToAuthority = db.getCollection("postcodeToAuthority")
  private val authorityBoundaries = db.getCollection("authorityBoundaries")

  def authenticate(username: String, password: String) {
    db.authenticate(username, password)
  }

  def dropAll() {
    dropAddresses()
    dropStreets()
    dropCodePoint()
  }

  def dropAddresses() {
    addresses.drop()
  }

  def dropCodePoint() {
    streets.drop()
  }

  def dropStreets() {
    postcodeToAuthority.drop()
  }

  def insertAddresses(things: List[DBObject]) = {
    println("INSERTING "  + things.size)
    addresses.insert(things.toArray, WriteConcern.Normal)
  }

  def insertStreets(things: List[DBObject]) = {
    streets.insert(things.toArray, WriteConcern.Normal)
  }

  def insertCodePoints(things: List[DBObject]) = {
    postcodeToAuthority.insert(things.toArray, WriteConcern.Normal)
  }

  def boundaryLineForGssCode(gssCode: String, x: Double, y: Double) = {
    val point = MongoDBObject("$geometry" -> MongoDBObject("type" -> "Point", "coordinates" -> GeoCoords(x, y)))
    val b = authorityBoundaries.findOne(DBObject("properties.CODE" -> gssCode, "geometry" -> MongoDBObject("$geoIntersects" -> point)))

    if (b == null) {
      None
    } else {
      Some(grater[AuthorityBoundary].asObject(b))
    }
  }

  def boundaryLineForLatLong(x: Double, y: Double) = {
    val point = MongoDBObject("$geometry" -> MongoDBObject("type" -> "Point", "coordinates" -> GeoCoords(x, y)))
    val b = authorityBoundaries.findOne(DBObject("geometry" -> MongoDBObject("$geoIntersects" -> point)))
    if (b == null) {
      None
    } else {
      Some(grater[AuthorityBoundary].asObject(b))
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
    val a = postcodeToAuthority.findOne(DBObject("postcode" -> postcode.toLowerCase.replaceAll(" ", "")))
    if (a == null) {
      None
    } else {
      Some(grater[CodePoint].asObject(a))
    }
  }

  def addAddressIndexes() {
    logger.info("indexing geo lookups - needs mongo 2.6")
    //authorityBoundaries.ensureIndex(DBObject("properties.CODE" -> 1))

    logger.info("indexing postcode")
    addresses.ensureIndex(DBObject("postcode" -> 1))

    logger.info("indexing uprn")
    addresses.ensureIndex(DBObject("uprn" -> 1))

    logger.info("indexing postcode, electoral")
    addresses.ensureIndex(DBObject("postcode" -> 1, "details.isElectoral" -> 1))
    addresses.ensureIndex(DBObject("postcode" -> 1, "details.isPostalAddress" -> 1, "presentation.property" -> 1,"presentation.street" -> 1,"presentation.town" -> 1,"presentation.area" -> 1,"presentation.locality" -> 1,"presentation.postcode" -> 1,"uprn" -> 1,"gssCode" -> 1), DBObject("name" ->  "postcode_presentation_idx"))

    logger.info("indexing gssCode")
    addresses.ensureIndex(DBObject("gssCode" -> 1))
  }

  def addStreetIndexes() {
    logger.info("indexing usrn on street")
    streets.ensureIndex(DBObject("usrn" -> 1))
  }

  def addCodePointIndexes() {
    logger.info("indexing postcode on codepoint")
    postcodeToAuthority.ensureIndex(DBObject("postcode" -> 1))
    postcodeToAuthority.ensureIndex(DBObject("gssCode" -> 1))
  }
}
