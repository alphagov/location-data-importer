package uk.gov.gds.location.importer.logging

import org.slf4j.LoggerFactory

trait Logging {
  val logger = LoggerFactory.getLogger(this.getClass)
}