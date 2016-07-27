object GCDLCM {
  def gcdiv(a: Int, b: Int): Int = {
    b match {
      case 0 => a
      case _ => gcdiv(b, (a % b))
    }
  }

  def lcmul(a:Int,b:Int):Int = {
    a*b / gcdiv(a,b)
  }

  def main(args: Array[String]) {
    println(gcdiv(32,40))
    println(lcmul(3,9))
  }
}
