object Factorial {
  def fac( n: Int): Int = { 
  facRec(1,n)
    }

def facRec(f:Int, i:Int):Int = {
if (i>0)
   facRec(f*i,i-1)
else
   f
}

/*  private int factorialIter(int f, int i) {
    while (i > 0) {
      f = f*i;
      i = i-1;
    }
    return f;
  }
    */
    
  def main(args: Array[String]) {
    println(fac(3) + " " + fac(4))
  }
}
