object GCDLCM {
  def gcdiv( a: Int,b:Int): Int = { 
    gcdRec(a,a,b)
    //gcdIter(a,a,b)
    }
    
  def gcdRec(varg:Int,a:Int,b:Int):Int = {
  if (b != 0)
      gcdRec(b,b,a%b)
   else
      a
}

  /* def gcdIter(var g:Int,a:Int,b:Int):Int = {
      while ( b != 0) {
      g = b
       b = a%b
      a = g
      }
      a
}*/

   def lcmul(a:Int,b:Int):Int = {
   var y: Int = a*b
   var z: Int = gcdiv(a,b)
   y/z
}

  def main(args: Array[String]) {
    println(gcdiv(32,40))
    println(lcmul(3,9))
  }
}
