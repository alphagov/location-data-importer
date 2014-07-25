package uk.gov.gds.location.importer.encryption

import org.joda.time.DateTime
import java.security.SecureRandom
import javax.crypto.spec.SecretKeySpec
import java.security.Key
import javax.crypto.Cipher
import javax.crypto.spec.IvParameterSpec
import org.apache.commons.codec.binary.Base64

case class AesEncryptionProduct(encryptedContent: Array[Byte], initializationVector: Array[Byte]) {

  def getEncryptedContent: Array[Byte] = encryptedContent

  def getInitializationVector: Array[Byte] = initializationVector

  def encryptedAsBase64String = Base64.encodeBase64String(encryptedContent)

  def initializationVectorAsBase64String = Base64.encodeBase64String(initializationVector)
}

object AesEncryptionService {
  private final val random: SecureRandom = new SecureRandom
  private final val algorithm: String = "AES/CBC/PKCS5Padding"
  random.setSeed(DateTime.now.getMillis)

  private def aesKeyFromBase64EncodedString(key: String): SecretKeySpec = new SecretKeySpec(Base64.decodeBase64(key), "AES")

  def byteArrayAsBase64String(bytes: Array[Byte]) = Base64.encodeBase64String(bytes)

  def encrypt(content: String, aesKey: String, ivSpec: IvParameterSpec): AesEncryptionProduct = {
    try {
      val cipher: Cipher = Cipher.getInstance(algorithm)
      cipher.init(Cipher.ENCRYPT_MODE, aesKeyFromBase64EncodedString(aesKey), ivSpec)
      AesEncryptionProduct(cipher.doFinal(content.getBytes), ivSpec.getIV)
    }
    catch {
      case ex: Exception => {
        throw new Exception("AES encryption failed", ex)
      }
    }
  }

  def decrypt(encryptedContent: Array[Byte], aesKey: String, iv: Array[Byte]): String = {
    try {
      val cipher: Cipher = Cipher.getInstance(algorithm)
      cipher.init(Cipher.DECRYPT_MODE, aesKeyFromBase64EncodedString(aesKey), new IvParameterSpec(iv))
      new String(cipher.doFinal(encryptedContent))
    }
    catch {
      case ex: Exception => {
        throw new Exception("AES decryption failed", ex)
      }
    }
  }

  def generateInitializationVector: IvParameterSpec = {
    val iv: Array[Byte] = new Array[Byte](16)
    random.nextBytes(iv)
    new IvParameterSpec(iv)
  }
}
