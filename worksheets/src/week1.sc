import scala.annotation.tailrec
import scala.util.Random

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var sum = 0
  for (i <- s until t) {
    sum = sum + scala.math.floor(scala.math.pow(a(i), p)).toInt
  }

  sum
}

sumSegment(Array(1, 2, 3), 2.0, 0, 2)


def mcCount(iter: Int) = {
  val randomX = new Random
  val randomY = new Random

  var hits = 0
  for (i <- 0 until iter) {
    val x = randomX.nextDouble() // [0, 1]
    val y = randomY.nextDouble() // [0, 1]
    // if point (x, y) is within a circle
    // that means its distance from the center
    // is less than radius (which is 1)
    if (x*x + y*y < 1) hits = hits + 1
  }

  hits
}

def monteCarloPiSeq(iter: Int) = 4.0 * mcCount(iter) / iter

monteCarloPiSeq(10000000)


trait Task[A] {
  def join: A
}

def task[A](c: => A): Task[A]

implicit def getJoin[T](x: Task[T]):T = x.join

def parallel[A, B](ca: => A, cb: => B): (A, B) = {
  (task(ca).join, task(cb).join)
}


def multiply(a: Int, b: Int): Int = {
  @tailrec
  def multiplyRec(total: Int, a: Int, b: Int): Int = {
    val smaller = math.min(a, b)
    val bigger = math.max(a, b)

    if (smaller == 0) total
    else multiplyRec(total + bigger, bigger, smaller - 1)
  }

  if (a == 0 || b == 0) 0
  else math.signum(a) * math.signum(b) * multiplyRec(0, math.abs(a), math.abs(b))
}

multiply(5, 3)
multiply(5, -3)
multiply(-5, -3)
multiply(-5, 1)
multiply(1, 10)
multiply(-45, -14)

