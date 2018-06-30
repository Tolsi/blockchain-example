package ru.tolsi.blockchain

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Status, Terminated}
import akka.pattern._
import akka.util.Timeout

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object BlockChainNodeActor {
  def props(startBlockChain: BlockChain, knownPeers: Seq[ActorRef]): Props = Props(new BlockChainNodeActor(startBlockChain, knownPeers))

  case object GetBlockChain

  case object GetConnectedPeers

  case class ConnectTo(peer: ActorRef)

  case object HandshakeRequest

  case object HandshakeResponse

  case object GetScore

  case class Score(score: Long)

  case object Sync

}

class BlockChainNodeActor(startBlockChain: BlockChain, startKnownPeers: Seq[ActorRef]) extends Actor with ActorLogging {

  import BlockChainNodeActor._
  import context.dispatcher

  implicit val timeout: Timeout = Timeout(10 seconds)

  private def startConnection(to: ActorRef) = {
    to ! HandshakeRequest
  }

  override def preStart(): Unit = {
    context.become(active(startBlockChain, startKnownPeers))
    startKnownPeers.foreach(startConnection)

    context.system.scheduler.schedule(3 seconds, 3 seconds, self, Sync)
  }

  private def addPeer(blockChain: BlockChain, peers: Seq[ActorRef], peer: ActorRef) = {
    context.watch(peer)
    context.become(active(blockChain, peers :+ peer))
  }

  private def active(blockChain: BlockChain, peers: Seq[ActorRef]): Receive = {
    case GetConnectedPeers =>
      val s = sender()
      s ! peers
    case HandshakeRequest =>
      val s = sender()
      s ! HandshakeResponse
      addPeer(blockChain, peers, s)
    case GetBlockChain =>
      val s = sender()
      s ! blockChain
    case ConnectTo(node) =>
      startConnection(node)
    case HandshakeResponse =>
      addPeer(blockChain, peers, sender())
    case Terminated(terminatedNode) =>
      log.debug(s"Terminated $terminatedNode, remove from peers")
      context.become(active(blockChain, peers.filter(_ != terminatedNode)))
    case b: Block => blockChain.append(b) match {
      case Success(newBlockChain) =>
        context.become(active(newBlockChain, peers))
        peers.foreach(_.tell(b, self))
      case Failure(f) =>
        println(s"Error on apply block $b to blockchain $blockChain: $f")
    }
    case GetScore =>
      sender() ! Score(blockChain.score)
    case Sync =>
      def syncWith(node: ActorRef, blockChain: BlockChain, peers: Seq[ActorRef]) = {
        context.become(syncing(blockChain, peers))
        (node ? GetBlockChain).mapTo[BlockChain].map(bc => node -> bc).pipeTo(self)
      }

      val scoreFutures = peers.map(p => (p ? GetScore).mapTo[Score].map(s => p -> s))
      Future.sequence(scoreFutures).map(seq => {
        val (topBlockChainNode, topBlockChainScore) = seq.maxBy(_._2.score)
        if (topBlockChainScore.score > blockChain.score) {
          syncWith(topBlockChainNode, blockChain, peers)
        }
      })
  }

  private def goActive(blockChain: BlockChain, peers: Seq[ActorRef], stash: Queue[(Any, ActorRef)]): Unit = {
    log.debug(s"Going active")
    context.become(active(blockChain, peers))
    stash.foreach(Function.tupled(self.tell))
  }

  private def syncing(blockChain: BlockChain, peers: Seq[ActorRef], stash: Queue[(Any, ActorRef)] = Queue.empty): Receive = {
    case (from: ActorRef, foreignBlockChain: BlockChain) =>
      if (foreignBlockChain.score > blockChain.score) {
        val foreignBlockChainSeq = foreignBlockChain.toSeq
        blockChain.lastBlock match {
          case Some(lastBlock) =>
            val index = foreignBlockChainSeq.indexOf(lastBlock)

            if (foreignBlockChain.isValid) {
              goActive(foreignBlockChain, peers, stash)
            } else {
              log.warning(s"Invalid blockchain from $from")
              goActive(blockChain, peers.filter(_ != from), stash)
            }
          case None => goActive(foreignBlockChain, peers, stash)
        }
      } else {
        goActive(blockChain, peers, stash)
      }
    case Status.Failure(f) =>
      log.warning(s"Sync failure: ${f.getMessage}")
      goActive(blockChain, peers, stash)
    case msg =>
      context.become(syncing(blockChain, peers, stash :+ (msg -> sender())))
  }

  override def receive: Receive = Actor.emptyBehavior
}
