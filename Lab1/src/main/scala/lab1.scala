// 1.2
def multiHello(n:Int) =
    for (i <- 1 to n) 
        if (i%2==0) then println("Hello " + i) else println("Hello " + (n-i)) 

// 1.3
def splitByIndex(numbers:Seq[Int]): (Seq[Int], Seq[Int]) =
    (numbers.zipWithIndex.filter(_._1 % 2 == 0).map(_._1), 
    numbers.zipWithIndex.filter(_._1 % 2 != 0).map(_._1))


def maxValue(numbers:Seq[Int]): Int =
    numbers.reduce((a, b) => if (a>b) a else b)


@main def lab1()=
    multiHello(10)
    println(splitByIndex(List(1,2,123,4)))
    println(maxValue(List(1,2,123,4)))
    
    // 1.4
    var a = maxValue
    println(a)


// 1.5
// Pattern-matching 
def identifyType(x: Any): String = x match {
    case i: Int => s"Целое число: ${i}"
    case d: Double => f"Число с плавающей точкой: $d%.2f"
    case s: String => s"Строка длиной ${s.length}"
    case _ => "Неизвестный тип"
  }

@main def patternMatching() =
  println(identifyType(42))
  println(identifyType(3.14159))
  println(identifyType("Scala"))


// 1.6
// Композиция функций
def compose(f:Int=>Int, g:Int=>Int):Int=>Int = 
  x=>f(g(x))

@main def composition()=
  val f = (x:Int) => x * 2
  val g = (x:Int) => x / 4
  println(compose(f, g)(48))