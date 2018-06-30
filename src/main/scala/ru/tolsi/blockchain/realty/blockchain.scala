package ru.tolsi.blockchain.realty

import java.util.UUID

import ru.tolsi.blockchain.realty.BlockChain._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey, Signature}

import scala.util.Try

object Block {
  def forge(parent: String, transactions: Seq[Transaction]): Block = {
    Block(Some(parent), randomId, transactions)
  }
}

case class Block(parent: Option[String], id: String, transactions: Seq[Transaction])

trait Transaction {
  def id: String
}

object CreateRealty {
  def sign(id: Id, owner: Address, realtyAddress: RealtyAddress, pk: PrivateKey): CreateRealty = {
    val b = bytes(id, owner, realtyAddress)
    val signature = Curve25519.sign(pk, b)
    CreateRealty(id, owner, realtyAddress, signature)
  }

  def bytes(id: Id, owner: Address, realtyAddress: RealtyAddress): Array[Byte] = id.getBytes ++ owner.getBytes ++ realtyAddress.getBytes
}

case class CreateRealty(id: Id, owner: Address, realtyAddress: RealtyAddress, signature: Signature) extends Transaction

object TransferRealty {

  def sign(id: Id, newOwner: Address, realtyAddress: RealtyAddress, pk: PrivateKey): TransferRealty = {
    val b = bytes(id, newOwner, realtyAddress)
    val signature = Curve25519.sign(pk, b)
    TransferRealty(id, signature, newOwner, realtyAddress)
  }

  def bytes(id: Id, newOwner: Address, realtyAddress: RealtyAddress): Array[Byte] =
    id.getBytes ++ newOwner.getBytes ++ realtyAddress.getBytes
}

case class TransferRealty(id: Id, ownerSignature: Signature, newOwner: Address, realtyAddress: RealtyAddress) extends Transaction

object BlockChain {
  type Id = String
  type Address = String
  type RealtyAddress = String

  val genesis: Block = Block(None, randomId, Seq.empty)

  val ministryOfRealEstate: (PrivateKey, PublicKey) = Curve25519.createKeyPair(Array[Byte](1, 6, 3, 9, 3))

  def realtyAddressHash(realtyAddress: String): RealtyAddress = Base58.encode(Blake2b256.hash(realtyAddress))

  def randomId: String = UUID.randomUUID().toString

  def addressFromPublicKey(publicKey: PublicKey): String = Base58.encode(publicKey)

  def valid(tx: Transaction, current: BlockChain): Boolean = {
    tx match {
      case CreateRealty(id, owner, address, signature) =>
        val isValidMinistrySignature = Curve25519.verify(signature, CreateRealty.bytes(id, owner, address), ministryOfRealEstate._2)
        isValidMinistrySignature
      case TransferRealty(id, ownerOrMinistrySignature, newOwner, realtyAddress) =>
        val isRealtyExists = current.realtyExists(realtyAddress)
        val isValidMinistrySignature = Curve25519.verify(ownerOrMinistrySignature, TransferRealty.bytes(id, newOwner, realtyAddress), ministryOfRealEstate._2)
        val isValidOwnerSignature = current.owner(realtyAddress).map(owner => Curve25519.verify(ownerOrMinistrySignature, TransferRealty.bytes(id, newOwner, realtyAddress), PublicKey(Base58.decode(owner).get))).get
        isRealtyExists && (isValidMinistrySignature || isValidOwnerSignature)
    }
  }

  def valid(current: BlockChain, block: Block): Boolean = {
    val isTransactionsValid = block.transactions.forall(tx => valid(tx, current))
    val isLinkToLastBlock = current.lastOption.exists(_.id == block.parent.get)
    val isNeedGenesisBlock = current.isEmpty && block.parent.isEmpty
    val isIdValid = Try {
      UUID.fromString(block.id)
    }.isSuccess

    isTransactionsValid && (isLinkToLastBlock || isNeedGenesisBlock) && isIdValid
  }
}

case class BlockChain(private val blocks: Seq[Block] = Seq.empty, owners: Map[RealtyAddress, Address] = Map.empty) extends Iterable[Block] {
  def owner(address: RealtyAddress): Option[Address] = owners.get(address)
  def realtyExists(address: RealtyAddress): Boolean = owners.contains(address)

  private def updatedOwners(b: Block): Map[RealtyAddress, Address] = {
    b.transactions.filter(_.isInstanceOf[CreateRealty]).map(_.asInstanceOf[CreateRealty]).map(t => t.realtyAddress -> t.owner).toMap
  }

  def append(b: Block): Either[String, BlockChain] = {
    if (valid(this, b)) {
      Right(BlockChain(blocks :+ b, owners ++ updatedOwners(b)))
    } else {
      Left(s"Block '${b.id}' is invalid")
    }
  }

  override def iterator: Iterator[Block] = blocks.iterator
}

object RealtyApp extends App {
  val (vasyaPrivate, vasyaPublic) = Curve25519.createKeyPair(Array[Byte](2, 6, 4, 9, 1))
  val (petyaPrivate, petyaPublic) = Curve25519.createKeyPair(Array[Byte](1, 6, 4, 2, 0, 14))

  val realtyAddress = "Ульяновск, ул. Ленина, д. 1, кв. 23"
  val create1 = CreateRealty.sign(randomId, addressFromPublicKey(vasyaPublic), realtyAddressHash(realtyAddress), BlockChain.ministryOfRealEstate._1)
  val createInvalid2 = CreateRealty.sign(randomId, addressFromPublicKey(vasyaPublic), realtyAddressHash(realtyAddress), vasyaPrivate)

  val blockChain = BlockChain()

  val blockChainWithGenesis = blockChain.append(genesis).right.get

  val block = Block.forge(genesis.id, Seq(create1))

  val blockchainWithSomeRealty = blockChainWithGenesis.append(block).right.get

  val realtyHash = realtyAddressHash(realtyAddress)

  val vasyaToPetyaTransaction = TransferRealty.sign(randomId, addressFromPublicKey(petyaPublic), realtyHash, ministryOfRealEstate._1)

  val blockchainWithRealtyTransfer = blockchainWithSomeRealty.append(Block.forge(blockchainWithSomeRealty.last.id, Seq(vasyaToPetyaTransaction)))

  println(blockchainWithRealtyTransfer)
}