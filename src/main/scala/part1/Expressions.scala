package part1

import scala.annotation.tailrec

object Expressions extends App {

  def greetings(name: String, age: Int): String = {
    "Hi, my name is " + name + " and i am " + age + " years old"
  }

  def factorialFunction(n: Int): Int = {
    if(n==1 | n==0) 1
    else n*factorialFunction(n-1)
  }

  def fibonacciFunction(n: Int): Int = {
    @tailrec
    def fibonacciOperation(index: Int, prev: Int, current: Int): Int = {
      if (index<=0) current
      else fibonacciOperation(index - 1, prev+current, prev)
    }

    fibonacciOperation(n, 1, 0)

  }

  def primeNumber(n: Int): Boolean = {
    @tailrec
    def operationRestDivision(n: Int, division: Int): Boolean = {
      if (division<2) true
      else if(n%division==0) false
      else operationRestDivision(n, division-1)
    }

    operationRestDivision(n, n-1)

  }

//  println(greetings("Bruno", 27))
  println(factorialFunction(6))
  println(fibonacciFunction(9))
  println(primeNumber(629))

  def concatenateString(n: Int, string: String): String = {
    @tailrec
    def concatenateStringhelper(n: Int, accumulator: String): String = {
      if (n == 1) accumulator
      else concatenateStringhelper(n - 1, string + accumulator)
    }
    concatenateStringhelper(n - 1, string + string)
  }

  println(concatenateString(7, "Bruno"))

}
