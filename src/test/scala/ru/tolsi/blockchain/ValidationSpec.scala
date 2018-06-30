package ru.tolsi.blockchain

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import ru.tolsi.blockchain.BlockChainNodeActor.GetBlockChain

class ValidationSpec extends TestKit(ActorSystem("ValidationSpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An BlockChain node actor" must {
    "accept valid block" in {
      val bc = BlockChain.withGenesis
      val node = system.actorOf(BlockChainNodeActor.props(bc, Seq.empty))

      val blockChainWithNewBlock = bc.append(Block.forge(bc.lastBlockId)).get

      node ! blockChainWithNewBlock.blocks.values.last
      node ! GetBlockChain

      expectMsg(blockChainWithNewBlock)
    }

    "not accept second genesis block" in {
      val bc = BlockChain.withGenesis
      val node = system.actorOf(BlockChainNodeActor.props(bc, Seq.empty))

      val invalidBlock = Block.forge()

      node ! invalidBlock
      node ! GetBlockChain
      expectMsg(bc)
    }

    "not accept invalid by id block" in {
      val bc = BlockChain.withGenesis
      val node = system.actorOf(BlockChainNodeActor.props(bc, Seq.empty))

      val invalidBlock = Block.forge(bc.lastBlockId).copy(id = "123")

      node ! invalidBlock
      node ! GetBlockChain
      expectMsg(bc)
    }

    "not accept invalid by score block" in {
      val bc = BlockChain.withGenesis
      val node = system.actorOf(BlockChainNodeActor.props(bc, Seq.empty))

      val invalidBlock = Block.forge(bc.lastBlockId).copy(score = -1)

      node ! invalidBlock
      node ! GetBlockChain
      expectMsg(bc)
    }

    "not accept by parent block not in blockchain" in {
      val bc = BlockChain.withGenesis
      val node = system.actorOf(BlockChainNodeActor.props(bc, Seq.empty))

      val invalidBlock = Block.forge(Some("30a6eb1e-f2c6-41ed-b26d-571999019f8d"))

      node ! invalidBlock
      node ! GetBlockChain
      expectMsg(bc)
    }

    "not accept by invalid parent block" in {
      val bc = BlockChain.withGenesis
      val node = system.actorOf(BlockChainNodeActor.props(bc, Seq.empty))

      val invalidBlock = Block.forge(Some("345213"))

      node ! invalidBlock
      node ! GetBlockChain
      expectMsg(bc)
    }
  }
}