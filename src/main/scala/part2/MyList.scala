package part2

import scala.language.postfixOps

abstract class MyList[+A] {

  def head : A
  def tail : MyList[A]
  def isEmpty : Boolean
  def add[B>:A](element: B) : MyList[B]
  def printElements : String
  override def toString : String = "String = [ " + printElements + " ]"

  def map[B](transformer: (A) => B): MyList[B]
  def filter(predicate: (A) => Boolean): MyList[A]
  def flatMap[B](transformer: (A) => MyList[B]): MyList[B]

  def ++[B >: A](myList: MyList[B]) : MyList[B]

}

case object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: MyList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def add[B>:Nothing](element: B): MyList[B] = GenericList(element, this)
  override def printElements: String = ""
  override def map[B](transformer: (Nothing) => B): MyList[B] = Empty
  override def filter(predicate: (Nothing) => Boolean): MyList[Nothing] = Empty
  override def flatMap[B](transformer: (Nothing) => MyList[B]): MyList[B] = Empty
  override def ++[B >: Nothing](myList: MyList[B]) : MyList[B] = myList
}

case class GenericList[+A](h: A, t: MyList[A]) extends MyList[A] {

  val listString = s"${h}"

  override def head: A = this.h

  override def tail: MyList[A] = this.t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): MyList[B] = GenericList(element, this)

  override def printElements: String = {
    if(this.tail.isEmpty) s"${listString}"
    else s"${listString} ${this.tail.printElements}"
  }

  def filter(predicate: (A) => Boolean): MyList[A] = {
    if(predicate(h)) GenericList(h, t.filter(predicate))
    else t.filter(predicate)
  }

  def map[B](transformer: (A) => B): MyList[B] = {
    GenericList(transformer(h), t.map(transformer))
  }

  override def flatMap[B](transformer: (A) => MyList[B]): MyList[B] = {
    transformer(h) ++ t.flatMap(transformer)
  }

  override def ++[B >: A](myList: MyList[B]): MyList[B] = {
    GenericList[B](h, tail ++ myList)
  }
}

object TestMyList extends App {
  val integerList: GenericList[Int] = GenericList(1, GenericList(2, GenericList(3, GenericList(5, Empty))))
  println(integerList.tail.head)
  println(integerList.isEmpty)

  val newIntegerList = integerList.add(9)

  val doubleList: GenericList[Double] = GenericList(1.11, GenericList(2.33, GenericList(3.66, GenericList(5.55, Empty))))
  println(doubleList.toString)

  val stringList = GenericList("Bruno", GenericList("Myllena", GenericList(2, Empty)))

  val myListString = newIntegerList.toString
  println(myListString)
  println(stringList)

  // Use of Function example (much less code)
  val functionMap: Int => Int = _ * 2
  println(newIntegerList.map(functionMap).toString)

  // Use of Function example (much less code)
  println(newIntegerList.filter((element: Int) => element % 3 == 0).toString)

  println(newIntegerList.flatMap((element: Int) => GenericList(element, GenericList(element + 1, Empty))).toString)
}
