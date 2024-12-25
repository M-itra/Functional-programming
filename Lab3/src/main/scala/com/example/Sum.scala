package com.example

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem


object Server{
    type Addable = Int | Double | String
    case class AddMessage(a:Addable, b:Addable, replyTo:ActorRef[Addable])
    
    def apply[A]():Behavior[AddMessage] = Behaviors.receive{
        (context, message)=>
            message.replyTo ! {(message.a, message.b) match
                case (a:String, b:Addable) => a + b
                case (a:Addable, b:String) => b.prependedAll(a.toString())
                case (a:Int, b:Int) => a + b
                case (a:Double, b:Double) => a + b
                case _ => throw new IllegalArgumentException("Unsupported types")
            }
        context.log.info(message.a.toString() + " + " +  message.b.toString())
        Behaviors.same
    }
}


object Client{
    def apply(server: ActorRef[Server.AddMessage]):Behavior[Server.Addable] = Behaviors.setup{ (context)=>
        
        server ! Server.AddMessage(3, 8, context.self)
        server ! Server.AddMessage(101.5, 32.1, context.self)
        server ! Server.AddMessage(121, 500, context.self)
        server ! Server.AddMessage("Hello ", "world", context.self)

        Behaviors.receive{ (context, message) =>
            context.log.info(message.toString())
            Behaviors.same
        }
    }
}


object System {
  def apply(): Behavior[Unit] = Behaviors.setup { context =>
        val server = context.spawn(Server(), "server")
        context.spawn(Client(server), "client")
        
        Behaviors.same
    }
}


@main def start(): Unit =
    val system = ActorSystem(System(),"system")
