package ru.tolsi.blockchain

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import ru.tolsi.blockchain.BlockChainNodeActor.{ConnectTo, GetBlockChain}

import scala.concurrent.duration._

class SimpleSyncSpec extends TestKit(ActorSystem("BlockChainNodeSpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An BlockChain node actor" must {
    "synchronize with a known node" in {
      val blockChainWithOnlyGenesis = BlockChain.withGenesis
      val blockChainWithNewBlock = blockChainWithOnlyGenesis.append(Block.forge(blockChainWithOnlyGenesis.lastBlockId)).get

      val nodeWithNewBlock = system.actorOf(BlockChainNodeActor.props(blockChainWithNewBlock, Seq.empty))
      val node = system.actorOf(BlockChainNodeActor.props(blockChainWithOnlyGenesis, Seq(nodeWithNewBlock)))

      expectNoMessage(5.second)

      node ! GetBlockChain

      expectMsg(blockChainWithNewBlock)
    }

    "synchronize with new outgoing connected node" in {
      val blockChainWithOnlyGenesis = BlockChain.withGenesis
      val blockChainWithNewBlock = blockChainWithOnlyGenesis.append(Block.forge(blockChainWithOnlyGenesis.lastBlockId)).get

      val nodeWithNewBlock = system.actorOf(BlockChainNodeActor.props(blockChainWithNewBlock, Seq.empty))
      val node = system.actorOf(BlockChainNodeActor.props(blockChainWithOnlyGenesis, Seq.empty))

      node ! ConnectTo(nodeWithNewBlock)

      expectNoMessage(5.second)

      node ! GetBlockChain
      expectMsg(blockChainWithNewBlock)
    }

    "synchronize with new incoming connected node" in {
      val blockChainWithOnlyGenesis = BlockChain.withGenesis
      val blockChainWithNewBlock = blockChainWithOnlyGenesis.append(Block.forge(blockChainWithOnlyGenesis.lastBlockId)).get

      val nodeWithNewBlock = system.actorOf(BlockChainNodeActor.props(blockChainWithNewBlock, Seq.empty))
      val node = system.actorOf(BlockChainNodeActor.props(blockChainWithOnlyGenesis, Seq.empty))

      nodeWithNewBlock ! ConnectTo(node)

      expectNoMessage(5.second)

      node ! GetBlockChain
      expectMsg(blockChainWithNewBlock)
    }

    "pick the best long chain" in {
      val blockChainWithOnlyGenesis = BlockChain.withGenesis

      def createChain(length: Int): BlockChain = {
        (0 to length).foldLeft(blockChainWithOnlyGenesis) {
          case (bc, _) => bc.append(Block.forge(bc.lastBlockId)).get
        }
      }

      val blockChain1 = createChain(10)
      val blockChain2 = createChain(10)

      val bestChain = Seq(blockChain1, blockChain2).maxBy(_.score)

      val nodeWithNewBlock1 = system.actorOf(BlockChainNodeActor.props(blockChain1, Seq.empty))
      val nodeWithNewBlock2 = system.actorOf(BlockChainNodeActor.props(blockChain2, Seq.empty))

      nodeWithNewBlock1 ! ConnectTo(nodeWithNewBlock2)

      expectNoMessage(5.second)

      nodeWithNewBlock1 ! GetBlockChain
      expectMsg(bestChain)

      nodeWithNewBlock2 ! GetBlockChain
      expectMsg(bestChain)
    }

    "synchronize network with the best chain in star topology" in {
      val blockChainWithOnlyGenesis = BlockChain.withGenesis

      val blockChainWithBestFork = blockChainWithOnlyGenesis.append(Block(blockChainWithOnlyGenesis.lastBlockId, Block.generateId, 500)).get
      val someOtherForks = Seq.fill(5)(blockChainWithOnlyGenesis.append(Block.forge(blockChainWithOnlyGenesis.lastBlockId)).get)

      val allForks = someOtherForks :+ blockChainWithBestFork

      val nodesWithDifferentBlockChains = allForks.map(bc =>
        system.actorOf(BlockChainNodeActor.props(bc, Seq.empty)))

      val starCenterNode = system.actorOf(BlockChainNodeActor.props(blockChainWithOnlyGenesis, Seq.empty))

      nodesWithDifferentBlockChains.foreach(node => node ! ConnectTo(starCenterNode))

      expectNoMessage(10.second)

      val allNodes = starCenterNode +: nodesWithDifferentBlockChains

      allNodes.foreach(node => {
        node ! GetBlockChain
        expectMsg(blockChainWithBestFork)
      })
    }

    "synchronize network with the best chain in chain topology" in {
      val blockChainWithOnlyGenesis = BlockChain.withGenesis

      val blockChainWithBestFork = blockChainWithOnlyGenesis.append(Block(blockChainWithOnlyGenesis.lastBlockId, Block.generateId, 500)).get
      val someOtherForks = Seq.fill(5)(blockChainWithOnlyGenesis.append(Block.forge(blockChainWithOnlyGenesis.lastBlockId)).get)

      val allForks = someOtherForks :+ blockChainWithBestFork

      val nodesWithDifferentBlockChains = allForks.map(bc =>
        system.actorOf(BlockChainNodeActor.props(bc, Seq.empty)))

      val node = system.actorOf(BlockChainNodeActor.props(blockChainWithOnlyGenesis, Seq.empty))

      val allNodes = node +: nodesWithDifferentBlockChains

      allNodes.sliding(2).foreach {
        case Seq(from, to) => from ! ConnectTo(to)
      }

      expectNoMessage(20.second)

      allNodes.foreach(node => {
        node ! GetBlockChain
        expectMsg(blockChainWithBestFork)
      })
    }
  }
}