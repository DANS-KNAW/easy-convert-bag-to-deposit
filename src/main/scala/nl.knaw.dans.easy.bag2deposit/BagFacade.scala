/*
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.bag2deposit

import better.files.File
import better.files.File.apply
import gov.loc.repository.bagit.creator.{ CreatePayloadManifestsVistor, CreateTagManifestsVistor }
import gov.loc.repository.bagit.domain
import gov.loc.repository.bagit.domain.Bag
import gov.loc.repository.bagit.hash.{ Hasher, StandardSupportedAlgorithms }
import gov.loc.repository.bagit.reader.BagReader
import gov.loc.repository.bagit.writer.{ ManifestWriter, MetadataWriter }

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ FileVisitResult, Files, Path }
import java.security.MessageDigest
import java.util
import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }

object BagFacade {

  // these constants duplicate nl.knaw.dans.bag.v0.DansV0Bag
  val IS_VERSION_OF_KEY = "Is-Version-Of"
  val EASY_USER_ACCOUNT_KEY = "EASY-User-Account"

  // duplicate of https://github.com/DANS-KNAW/easy-fedora-to-bag/blob/1cf9c03209/src/main/scala/nl/knaw/dans/easy/fedoratobag/BagVersion.scala#L30
  val BAG_SEQUENCE_NUMBER = "Bag-Sequence-Number"

  // TODO variant of https://github.com/DANS-KNAW/easy-ingest-flow/blob/78ea3bec23923adf10c1c0650b019ea51c251ce6/src/main/scala/nl.knaw.dans.easy.ingestflow/BagitFacadeComponent.scala#L133

  private val bagReader = new BagReader()

  def getBag(bagDir: File): Try[Bag] = {
    val triedBag = Try {
      bagReader.read(bagDir.path)
    }
    triedBag.recoverWith {
      case cause: Exception => Failure(InvalidBagException(s"$bagDir, $cause"))
    }
  }

  def updateMetadata(bag: Bag): Try[Unit] = {
    trace(bag.getRootDir)
    Try {
      MetadataWriter.writeBagMetadata(bag.getMetadata, bag.getVersion, bag.getRootDir, bag.getFileEncoding)
    }
  }

  private val includeHiddenFiles = true

  /**
   * (re)calculate values for all algorithms of new/changed payload files
   *
   * @param bag            changed bag
   * @param payloadEntries directory or file relatieve to the root of the bag
   * @param preStageds     files to be removed from the bag and the payload manifests
   * @return
   */
  def updatePayloadManifests(bag: Bag, payloadEntries: Path, preStageds: Seq[PreStaged]): Try[Unit] = Try {
    trace(bag.getRootDir)
    if (!payloadEntries.toString.startsWith("data/")) {
      throw new IllegalArgumentException(s"path must start with data, found $payloadEntries")
    }
    if (bag.getPayLoadManifests.isEmpty) {
      throw new IllegalArgumentException(s"No payload manifests found (as DansV0Bag would have created) ${ bag.getRootDir }")
    }
    val payloadManifests = bag.getPayLoadManifests
    val algorithms = payloadManifests.asScala.map(_.getAlgorithm).asJava
    val map = Hasher.createManifestToMessageDigestMap(algorithms)
    val visitor = new CreatePayloadManifestsVistor(map, includeHiddenFiles)
    Files.walkFileTree(bag.getRootDir.resolve(payloadEntries), visitor)
    mergeManifests(payloadManifests, map)

    val fileToChecksumMap = sha1Manifest(payloadManifests)
    val duplicateShasInManifest = fileToChecksumMap.values().asScala
      .groupBy(identity)
      .collect { case (x, List(_, _, _*)) => x }
      .toList
    val preStagedShas = preStageds
      .withFilter(p => !duplicateShasInManifest.contains(p.checksumValue))
      .map(p => p.checksumValue)
    val preStagedPaths = fileToChecksumMap.asScala
      .withFilter { case (_, checksum) => preStagedShas.contains(checksum) }
      .map { case (path, _) => path }
    preStagedPaths.foreach { path =>
      path.delete()
      fileToChecksumMap.remove(path)
    }
  }

  def sha1Manifest(payloadManifests: util.Set[domain.Manifest]): util.Map[Path, String] = {
    payloadManifests.asScala
      .find(m => m.getAlgorithm == StandardSupportedAlgorithms.SHA1)
      .map(_.getFileToChecksumMap)
      .getOrElse(throw new Exception("no SHA1 manifest in ${bag.getRootDir}"))
  }

  /** Recalculates the checksums for changed metadata files (and payload manifests) for all present algorithms */
  def updateTagManifests(bag: Bag): Try[Unit] = Try {
    val bagRoot = bag.getRootDir
    (bagRoot / "tagmanifest-sha1.txt").delete()
    val tagManifests = bag.getTagManifests
    val algorithms = tagManifests.asScala.map(_.getAlgorithm).asJava
    val map = Hasher.createManifestToMessageDigestMap(algorithms)
    val visitor = new CreateTagManifestsVistor(map, includeHiddenFiles)
    Files.walkFileTree(bagRoot, visitor)
    mergeManifests(tagManifests, map)
  }

  private def mergeManifests(manifests: util.Set[domain.Manifest], manifetsToDigest: util.Map[domain.Manifest, MessageDigest]): Unit = {
    val newMap = manifetsToDigest.keySet().asScala.map(m =>
      m.getAlgorithm -> m.getFileToChecksumMap
    ).toMap
    for {
      m <- manifests.asScala
      (path, hash) <- newMap(m.getAlgorithm).asScala
    } {
      m.getFileToChecksumMap.put(path, hash)
    }
  }

  /** (re)writes payload files for all present algorithms */
  def writePayloadManifests(bag: Bag): Try[Unit] = Try {
    val bagRoot = bag.getRootDir
    val encoding = bag.getFileEncoding
    ManifestWriter.writePayloadManifests(bag.getPayLoadManifests, bagRoot, bagRoot, encoding)
  }

  /** (re)writes tagmanifest files for all present algorithms */
  def writeTagManifests(bag: Bag): Try[Unit] = Try {
    val bagRoot = bag.getRootDir
    val encoding = bag.getFileEncoding
    ManifestWriter.writeTagManifests(bag.getTagManifests, bagRoot, bagRoot, encoding)
  }
}
