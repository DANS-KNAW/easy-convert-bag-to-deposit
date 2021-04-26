/**
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
import gov.loc.repository.bagit.creator.{ CreatePayloadManifestsVistor, CreateTagManifestsVistor }
import gov.loc.repository.bagit.domain.Bag
import gov.loc.repository.bagit.hash.Hasher
import gov.loc.repository.bagit.reader.BagReader
import gov.loc.repository.bagit.writer.{ ManifestWriter, MetadataWriter }

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ FileVisitResult, Files, Path }
import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }

object BagFacade {

  // these constants duplicate nl.knaw.dans.bag.v0.DansV0Bag
  val IS_VERSION_OF_KEY = "Is-Version-Of"
  val EASY_USER_ACCOUNT_KEY = "EASY-User-Account"

  // TODO add to dans-bag lib
  //  variant of https://github.com/DANS-KNAW/easy-ingest-flow/blob/78ea3bec23923adf10c1c0650b019ea51c251ce6/src/main/scala/nl.knaw.dans.easy.ingestflow/BagitFacadeComponent.scala#L133

  private val bagReader = new BagReader()

  def getBag(bagDir: File): Try[Bag] = Try {
    bagReader.read(bagDir.path)
  }.recoverWith {
    case cause: Exception => Failure(InvalidBagException(s"$bagDir, $cause"))
  }

  def updateMetadata(bag: Bag): Try[Unit] = Try {
    MetadataWriter.writeBagMetadata(bag.getMetadata, bag.getVersion, bag.getRootDir, bag.getFileEncoding)
  }

  private val includeHiddenFiles = true

  /**
   * (re)calculate values for all algorithms of new/changed payload files
   *
   * @param bag            changed bag
   * @param payloadEntries directory or file relatieve to the root of the bag
   * @return
   */
  def putPayloadManifests(bag: Bag, payloadEntries: Path): Try[Unit] = Try {
    if (!payloadEntries.toString.startsWith("data/")) {
      throw new IllegalArgumentException(s"path must start with data, found $payloadEntries")
    }
    if (bag.getPayLoadManifests.isEmpty) {
      throw new IllegalArgumentException(s"No payload manifests found (as DansV0Bag would have created) ${bag.getRootDir}")
    }
    val payloadManifests = bag.getPayLoadManifests
    val map = Hasher.createManifestToMessageDigestMap(
      payloadManifests.asScala.map(_.getAlgorithm).asJava
    )
    val manifestsVisitor = new CreatePayloadManifestsVistor(map, includeHiddenFiles)
    Files.walkFileTree(bag.getRootDir.resolve(payloadEntries), manifestsVisitor)
    val newMap = map.keySet().asScala.map(m =>
      m.getAlgorithm -> m.getFileToChecksumMap
    ).toMap
    for {
      m <- payloadManifests.asScala
      (path, hash) <- newMap(m.getAlgorithm).asScala
    } {
      m.getFileToChecksumMap.put(path, hash)
    }
  }

  /** Recalculates the SHAs for all metadata files for all present algorithms */
  def updateTagManifests(bag: Bag): Try[Unit] = Try {
    val bagRoot = bag.getRootDir

    def isTagManifest(path: Path): Boolean = {
      bagRoot.relativize(path).getNameCount == 1 && path.getFileName.toString.startsWith("tagmanifest-")
    }

    val algorithms = bag.getTagManifests.asScala.map(_.getAlgorithm).asJava
    val tagFilesMap = Hasher.createManifestToMessageDigestMap(algorithms)
    val tagVisitor = new CreateTagManifestsVistor(tagFilesMap, includeHiddenFiles) {
      override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
        // bagit doesn't skip it as during bag creation it is not there
        if (isTagManifest(path)) FileVisitResult.CONTINUE
        else super.visitFile(path, attrs)
      }
    }
    Files.walkFileTree(bagRoot, tagVisitor)
    bag.getTagManifests.clear()
    bag.getTagManifests.addAll(tagFilesMap.keySet())
  }

  /** (re)writes payload and tagmanifest files for all present algorithms */
  def writeManifests(bag: Bag): Try[Unit] = Try {
    val bagRoot = bag.getRootDir
    val encoding = bag.getFileEncoding
    ManifestWriter.writePayloadManifests(bag.getPayLoadManifests, bagRoot, bagRoot, encoding)
    ManifestWriter.writeTagManifests(bag.getTagManifests, bagRoot, bagRoot, encoding)
  }
}
