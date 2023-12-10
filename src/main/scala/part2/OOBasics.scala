package part2

import java.time.{LocalDate, Year}
import scala.annotation.tailrec

object OOBasics extends App {
  val writer = Writer(firstName = "Bruno", surname = "Nascimento", year = 1996)
  val novel = Novel("O Maluco", 2002, writer)
  println(writer.fullName() + " with " + novel.authorAge())
  println(s"${novel.name} is written by ${novel.author.firstName} ${novel.author.surname} (released on ${novel.yearOfRelease})")

  val novelCopy = novel.copyOfNovel(2013)
  println(s"A copy of ${novelCopy.name} was released on ${novelCopy.yearOfRelease} by ${novelCopy.author.firstName} ${novelCopy.author.surname}")

  val counter = Counter(4)
  val counterIncremented = counter.incrementCounter
  val counterDecremented = counter.decrementCounter
  println(s"Counter on ${counter.counter}")
  println(s"Counter on ${counter.incrementCounter.counter}")
  println(s"Counter on ${counterIncremented.incrementCounter.counter}")

  println(s"Counter on ${counter.counter}")
  println(s"Counter on ${counter.decrementCounter.counter}")
  println(s"Counter on ${counterDecremented.decrementCounter.counter}")

  println(s"Counter on ${counter.counter}")
  println(s"Counter on ${counter.incrementCounter(4).counter}")
  println(s"Counter on ${counter.counter}")
  println(s"Counter on ${counter.decrementCounter(4).counter}")
}

class Writer(val firstName: String, val surname: String, val year: Int) {

  def fullName(): String = {
    s"${this.firstName} ${this.surname}"
  }

}

class Novel(val name: String, val yearOfRelease: Int, val author: Writer) {
  def authorAge(): Int = {
    LocalDate.now().getYear - this.author.year
  }

  def isWrittenBy(): String = {
    s"${this.author.firstName} ${this.author.surname}"
  }

  def copyOfNovel(yearOfRelease: Int): Novel = {
    new Novel(this.name, yearOfRelease, this.author)
  }
}

class Counter(val counter: Int) {
  def currentCount(): Int = {
    this.counter
  }

  def incrementCounter: Counter = {
    new Counter(this.counter + 1)
  }

  def decrementCounter: Counter = {
    new Counter(this.counter - 1)
  }

  def incrementCounter(value: Int): Counter = {
    if(value <= 0) this
    else incrementCounter.incrementCounter(value - 1)
  }

  def decrementCounter(value: Int): Counter = {
    if(value <= 0) this
    else decrementCounter.decrementCounter(value - 1)
  }
}
