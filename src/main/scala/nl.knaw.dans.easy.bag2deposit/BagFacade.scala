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

  def addPayloadManifestEntries(bag: Bag, entries: Path): Try[Unit] = Try {
    val manifests = bag.getPayLoadManifests
    val map = Hasher.createManifestToMessageDigestMap(
      manifests.asScala.map(_.getAlgorithm).asJava
    )
    val bagDir = bag.getRootDir
    Files.walkFileTree(
      bagDir.resolve(entries),
      new CreatePayloadManifestsVistor(map, true),
    )
    val newEntries = map.keySet().asScala.map(x =>
      x.getAlgorithm -> x.getFileToChecksumMap
    ).toMap
    manifests.asScala.foreach{ m =>
      val newMap = newEntries(m.getAlgorithm).asScala
      val oldMap = m.getFileToChecksumMap.asScala.toMap // we don't want to recalculate these
      m.setFileToChecksumMap((newMap ++ oldMap).asJava)
    }
    ManifestWriter.writePayloadManifests(manifests, bagDir, bagDir, bag.getFileEncoding)
  }

  def updateTagManifest(bag: Bag): Try[Unit] = Try {
    def isTagManifest(path: Path): Boolean = {
      bag.getRootDir.relativize(path).getNameCount == 1 && path.getFileName.toString.startsWith("tagmanifest-")
    }

    val algorithms = bag.getTagManifests.asScala.map(_.getAlgorithm).asJava
    val tagFilesMap = Hasher.createManifestToMessageDigestMap(algorithms)
    val tagVisitor = new CreateTagManifestsVistor(tagFilesMap, true) {
      override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (isTagManifest(path)) FileVisitResult.CONTINUE
        else super.visitFile(path, attrs)
      }
    }
    val bagPath = bag.getRootDir
    Files.walkFileTree(bagPath, tagVisitor)
    bag.getTagManifests.clear()
    bag.getTagManifests.addAll(tagFilesMap.keySet())
    ManifestWriter.writeTagManifests(bag.getTagManifests, bagPath, bagPath, bag.getFileEncoding)
  }
}
