package ru.tolsi.blockchain

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern._
import akka.util.Timeout
import ru.tolsi.blockchain.BlockChainNodeActor.{ConnectTo, GetBlockChain, GetConnectedPeers}

import scala.concurrent.Future
import scala.concurrent.duration._

class BlockChainNode(val name: String, startBlockChain: BlockChain, knownPeers: Seq[ActorRef])(implicit context: ActorRefFactory) {
  private implicit val timeout: Timeout = Timeout(1 second)
  private val actorRef = context.actorOf(BlockChainNodeActor.props(startBlockChain, knownPeers), s"BlockChainNodeActor-$name")

  def getBlockСhain: Future[BlockChain] = (actorRef ? GetBlockChain).mapTo[BlockChain]

  def getConnectedPeers: Future[Seq[ActorRef]] = (actorRef ? GetConnectedPeers).mapTo[Seq[ActorRef]]

  def connectTo(peer: BlockChainNode): Future[Unit] = Future.successful(actorRef ! ConnectTo(peer.actorRef))

  def sendBlock(b: Block): Future[Unit] = Future.successful(actorRef ! b)
}
