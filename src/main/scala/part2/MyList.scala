package part2

import scala.language.postfixOps

abstract class MyList[+A] {

  def head : A
  def tail : MyList[A]
  def isEmpty : Boolean
  def add[B>:A](element: B) : MyList[B]
  def printElements : String
  override def toString : String = "String = [ " + printElements + " ]"

  def map[B](transformer: MyTransformer[A,B]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]
  def flatMap[B](transformer: MyTransformer[A,MyList[B]]): MyList[B]

  def ++[B >: A](myList: MyList[B]) : MyList[B]

}

case object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: MyList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def add[B>:Nothing](element: B): MyList[B] = GenericList(element, this)
  override def printElements: String = ""
  override def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty
  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty
  override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty
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

  def filter(predicate: MyPredicate[A]): MyList[A] = {
    if(predicate.test(h)) GenericList(h, t.filter(predicate))
    else t.filter(predicate)
  }

  def map[B](transformer: MyTransformer[A,B]): MyList[B] = {
    GenericList(transformer.transform(h), t.map(transformer))
  }

  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] = {
    transformer.transform(h) ++ t.flatMap(transformer)
  }

  override def ++[B >: A](myList: MyList[B]): MyList[B] = {
    GenericList[B](h, tail ++ myList)
  }
}

trait MyPredicate[-T] {
  def test(element: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(element: A): B
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

  println(newIntegerList.map(new MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }).toString)

  println(newIntegerList.filter(new MyPredicate[Int] {
    override def test(element: Int): Boolean = element%3==0
  }).toString)

  println(newIntegerList.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(element: Int): MyList[Int] = GenericList(element, GenericList(element+1, Empty))
  }).toString)
}
