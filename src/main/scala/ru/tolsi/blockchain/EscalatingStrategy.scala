package ru.tolsi.blockchain

import akka.actor.{AllForOneStrategy, SupervisorStrategy, SupervisorStrategyConfigurator}
import com.typesafe.scalalogging.StrictLogging

final class EscalatingStrategy extends SupervisorStrategyConfigurator with StrictLogging {
  override def create(): SupervisorStrategy = AllForOneStrategy(loggingEnabled = false) {
    case t: Throwable =>
      logger.error("Root actor got exception, escalate", t)
      SupervisorStrategy.Escalate
  }
}