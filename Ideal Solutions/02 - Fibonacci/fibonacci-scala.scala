object Fibonacci {
  def fib( n:Int) = fib_tr( n, 1, 0) 

  def fib_tr( n: Int, b: Int, a: Int): Int = n match {

    case 0 => a 
    case _ => fib_tr( n -1, a + b, b)
  }
}
