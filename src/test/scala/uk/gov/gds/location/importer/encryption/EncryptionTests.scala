package uk.gov.gds.location.importer.encryption

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import org.apache.commons.codec.binary.Base64

class EncryptionTests extends Specification with Mockito {

  val testKey = "QWVzS2B5QmVpbmdTb21lU3RyaW5nT2ZMZW5ndGgyNTY="

  "the encryption service" should {
    "decrypt an encrypted string" in {
      val ivSpec = AesEncryptionService.generateInitializationVector
      val encrypted = AesEncryptionService.encrypt("this string", testKey, ivSpec)
      AesEncryptionService.decrypt(encrypted.getEncryptedContent, testKey, encrypted.getInitializationVector) must beEqualTo("this string")
    }

    "decrypt an encrypted string with fields that have been base64 encoded" in {
      val ivSpec = AesEncryptionService.generateInitializationVector
      val encrypted = AesEncryptionService.encrypt("this string", testKey, ivSpec)
      AesEncryptionService.decrypt(Base64.decodeBase64(encrypted.encryptedAsBase64String), testKey, Base64.decodeBase64(encrypted.initializationVectorAsBase64String)) must beEqualTo("this string")
    }
  }
}
