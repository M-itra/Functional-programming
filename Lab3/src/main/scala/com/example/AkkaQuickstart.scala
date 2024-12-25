package com.example


import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.GreeterMain.SayHello

object Greeter {
  final case class Greet(whom: String, replyTo: ActorRef[Greeted]) // Сообщение, которое получает ответ.
  final case class Greeted(whom: String, from: ActorRef[Greet]) // Сообщение, которое отправляется в ответ.

  // Поведение актора
  // context: предоставляет контекст актора, включая логирование и доступ к его свойствам.
  // message: это сообщение, которое актор получает
  def apply(): Behavior[Greet] = Behaviors.receive { (context, message) =>

    // Актор логирует сообщение с приветствием.
    context.log.info("Hello {}!", message.whom)

    // После того как актор приветствовал, он отправляет ответное сообщение.
    message.replyTo ! Greeted(message.whom, context.self)
    
    // Поведение остается таким же, и актор готов принять следующее сообщение
    Behaviors.same
  }
}

// Актор GreeterBot, взаимодействует с актером Greeter и выполняет ограниченное
// количество приветствий, а затем завершает свою работу
object GreeterBot {

  // Поведение актора
  // Он принимает один параметр max, который задает максимальное количество приветствий, 
  // которые должен отправить
  def apply(max: Int): Behavior[Greeter.Greeted] = {
    bot(0, max)
  }

  // Основное поведение актора
  // greetingCounter: Счетчик отправленных приветствий
  // max: Максимальное количество приветствий
  private def bot(greetingCounter: Int, max: Int): Behavior[Greeter.Greeted] =
    Behaviors.receive { (context, message) =>
      val n = greetingCounter + 1
      // когда бот отправляет приветствие, он логирует это событие, отображая 
      // номер приветствия (n) и того, кого он приветствует
      context.log.info("Greeting {} for {}", n, message.whom)
      if (n == max) {
        Behaviors.stopped
      } else {
        // Бот отправляет сообщение типа Greeter.Greet обратно актеру Greeter
        message.from ! Greeter.Greet(message.whom, context.self)
        bot(n, max)
      }
    }
}

// Этот актор управляет двумя другими акторами — Greeter и GreeterBot
object GreeterMain {

  final case class SayHello(name: String) //будет использоваться для того, чтобы передать имя человека

  // Поведение актора
  def apply(): Behavior[SayHello] =
    // Behaviors.setup, позволяет создавать актор и выполнять начальную настройку

    Behaviors.setup { context =>
      // spawn — это метод, который создает нового актора и возвращает ссылку на него
      // В данном случае создается актор Greeter с именем "greeter"
      val greeter = context.spawn(Greeter(), "greeter")
      // Ожидание получения сообщения
      Behaviors.receiveMessage { message =>
        // создается актор GreeterBot, который будет отвечать на приветствия
        val replyTo = context.spawn(GreeterBot(max = 3), message.name)
        // отправление сообщения типа Greeter.Greet актеру greeter
        greeter ! Greeter.Greet(message.name, replyTo)
        Behaviors.same
      }
    }
}

// наследование от App, делает исполняемым приложением
object AkkaQuickstart extends App {
  // ActorSystem[GreeterMain.SayHello] указывает, что система ожидает, 
  // что GreeterMain будет обрабатывать сообщения типа GreeterMain.SayHello.

  // GreeterMain() создается при старте акторной системы и управляет Greeter и GreeterBot
  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")

  greeterMain ! SayHello("Charles")
}
