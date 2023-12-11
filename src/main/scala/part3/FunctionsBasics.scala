package part3

object FunctionsBasics extends App {

  val concatenateStrings = new Function2[String, String, String] {
    override def apply(element1: String, element2: String) = s"${element1}${element2}"
  }

  println(concatenateStrings("Bruno", "Myllena"))

  val specialMultiply: (Int) => ((Int) => Int) =
    (v1: Int) =>
    (v2: Int) => v2 * v1

  val multiply = specialMultiply(4)
  println(multiply(3))
  println(specialMultiply(4)(3)) // curried function

}
