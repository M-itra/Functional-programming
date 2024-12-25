package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorSystem


object Integral{
  case class Message(f:Double =>Double, 
    start:Double, 
    end:Double, 
    steps:Int, 
    replyTo:ActorRef[Double]
  )
  
  def apply[A]():Behavior[Message] = Behaviors.receive{ (context, message)=>
    val h = (message.end - message.start) / message.steps
    val resoult = (0 until message.steps).map(i => message.f(message.start + i * h) * h).sum
    context.log.info(s"Computed integral: $resoult")

    message.replyTo ! resoult
    Behaviors.same
  }
}


object SumIntegrals {
  case class AddResult(partialSum: Double, remainingResults: Int)

  def apply(): Behavior[AddResult] = Behaviors.setup { context =>
    var totalSum = 0.0

    Behaviors.receiveMessage {
      case AddResult(partialSum, remainingResults) =>
        totalSum += partialSum
      
      context.log.info(s"Partial sum: $partialSum, current total: $totalSum, remaining: $remainingResults")

      if (remainingResults == 0) {
          context.log.info(s"Final result: $totalSum")
      }
      Behaviors.same
    }
  }
}


object IntegrationSystem {
  case class StartCalculation(start: Double, 
    end: Double, 
    steps: Int, 
    f: Double => Double
  )

  def apply(): Behavior[StartCalculation] = Behaviors.setup { context =>
    val integral = context.spawn(Integral(), "integral")
    val sumIntegrals = context.spawn(SumIntegrals(), "sumIntegrals")

    def sendCalculation(
      f: Double => Double, 
      start: Double, 
      end: Double, 
      steps: Int, 
      replyTo: ActorRef[Double]
    ): Unit = {
      integral ! Integral.Message(f, start, end, steps, replyTo)
    }

    Behaviors.receiveMessage {
      case StartCalculation(start, end, steps, f) =>
        val numActors = 4
        val stepSize = (end - start) / numActors
        var remainingResults = numActors

        (0 until numActors).foreach { i =>
          val startOfActor = start + i * stepSize
          val endOfActor = if (i == numActors - 1) end else startOfActor + stepSize
          val numSteps = steps / numActors

          val replyTo = context.spawn(Behaviors.receiveMessage[Double] { partialSum =>
            remainingResults -= 1
            sumIntegrals ! SumIntegrals.AddResult(partialSum, remainingResults)
            Behaviors.same
          }, s"responseActor-${i}")

          sendCalculation(f, startOfActor, endOfActor, numSteps, replyTo)
        }

        Behaviors.same
    }
  }
}


@main def main(): Unit = {
  val system = ActorSystem(IntegrationSystem(), "system")

  def f(x: Double): Double = x * x - x/2
  system ! IntegrationSystem.StartCalculation(7, 32, 50000, f)
}