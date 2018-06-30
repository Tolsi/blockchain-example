package ru.tolsi.blockchain

import java.util.UUID

import scala.util.{Random, Try}

object Block {
  private val r = new Random()

  def forge(parentId: Option[String] = None, maxScore: Int = 100): Block = Block(parentId, generateId, 1 + r.nextInt(maxScore))

  def generateId: String = UUID.randomUUID().toString

  case class ValidationError(msg: String)

  def isValid(b: Block, bc: BlockChain): Either[ValidationError, Block] = {
    if (bc.contains(b)) {
      // already in blockchain
      Right(b)
    } else if (b.parentId.isEmpty && bc.lastBlockId.isEmpty && bc.height == 0) {
      // genesis block
      Right(b)
    } else if (Try(UUID.fromString(b.id)).isFailure) {
      Left(ValidationError("Invalid id"))
    } else if (b.score <= 0) {
      Left(ValidationError("Block score should be positive"))
    } else if (!b.parentId.exists(bc.lastBlockId.contains)) {
      Left(ValidationError("Block parent is not last in blockchain"))
    } else {
      // validation complete
      Right(b)
    }
  }
}

case class Block(parentId: Option[String], id: String, score: Long)