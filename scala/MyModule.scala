package mypackage

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(pos: Int): Boolean =
      if (pos >= as.length - 1) true
      else gt(as(pos), as(pos + 1)) && (loop(pos + 1))
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    def g(b: B): C =
      f(a, b)
    g
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def g(a: A): B => C = {
      def h(b: B): C =
        f(a, b)
      h
    }
    g
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    def h(a: A): C =
      f(g(a))
    h
  }


  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}

object AnotherModule {
  def hello = {
    println("ok")
  }
}