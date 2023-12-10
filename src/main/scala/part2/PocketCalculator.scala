package part2

case class PocketCalculator() {

  def add(x: Int, y: Int): Int = {
    val result = x+y
    if (x > 0 && y > 0 && result < 0) throw new OverflowException
    else if(x < 0 && y < 0 && result > 0) throw new UnderflowException
    else result
  }
  def subtract(x: Int, y: Int): Int = {
    val result = x - y
    if (x > 0 && y < 0 && result < 0) throw new OverflowException
    else if (x < 0 && y > 0 && result > 0) throw new UnderflowException
    else result
  }
  def multiply(x: Int, y: Int): Int = {
    val result = x * y
    if (x > 0 && y > 0 && result < 0) throw new OverflowException
    else if (x < 0 && y < 0 && result < 0) throw new OverflowException
    else if (x > 0 && y < 0 && result > 0) throw new UnderflowException
    else if (x < 0 && y > 0 && result > 0) throw new UnderflowException
    else result
  }
  def divide(x: Int, y: Int): Int = {
    if(y == 0) throw new MathCalculationException
    else x/y
  }

}

class OverflowException extends RuntimeException
class UnderflowException extends RuntimeException
class MathCalculationException extends RuntimeException("Divided By zero")

object TestPocketCalculator extends App {
  private val pocketCalculator = PocketCalculator()
  println(pocketCalculator.add(2, 5))
  println(pocketCalculator.subtract(55,23))
  println(pocketCalculator.multiply(3,6))
  println(pocketCalculator.divide(8, 2))
  try {
    pocketCalculator.divide(2, 0)
  } catch
    case e: MathCalculationException => println("Not allowed division by 0")

  pocketCalculator.divide(2, 0)
}
