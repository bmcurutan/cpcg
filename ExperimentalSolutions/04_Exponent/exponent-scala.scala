object Exponent {
  def expo( a: Int,b:Int): Int = { 
  expRec(1,a,b)
    }
    
def expRec(e:Int,a:Int,b:Int):Int = {
if (b != 0) 
   expRec(e*a,a,b-1)
else
e
}

/*  private int exponentIter(int e, int a , int b) {
    while (b != 0) {
      e = e * a;
      b = b-1;
    }
    return e;
  }
*/

  def main(args: Array[String]) {
    println(expo(2,3))
  }
}
