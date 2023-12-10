package part2

import scala.language.postfixOps

object MethodNotations extends App {

  val bruno = new Person("Bruno")
  println((bruno + "O Louco").name)

  val myllena = new Person("Myllena", 25)
  println(myllena.age)
  println((+myllena).age)

  println(bruno learnsScala)

  val janderson = new Person("Janderson", 54, "The Lord of the Rings")
  println(janderson(4))

  class Person(val name: String, val age: Int, val favoriteMovie: String) {
    def this(name: String) = {
      this(name, 0, "")
    }
    def this(name: String, age: Int) = {
      this(name, age, "")
    }
    def +(nickname: String): Person = new Person(s"${this.name} (${nickname})")
    def unary_+ : Person = new Person(this.name, this.age + 1)
    def learns(something: String): String = s"${this.name} learns ${something}"
    def learnsScala : String = learns("Scala")
    def apply(times: Int): String = s"${this.name} watched ${this.favoriteMovie} ${times} times"
  }

}


