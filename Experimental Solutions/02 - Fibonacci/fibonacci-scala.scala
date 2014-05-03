object Fibonacci {
  def fib( n: Int): Int = { 
  if (n < 2)
      n 
   else
       //fibRec(n,0,1,2,n)
       fibIter(n,0,1,2,n)
    }

def fibRec(f:Int,n1:Int,n2:Int,i:Int,n:Int):Int = {
   if (i <= n)
       fibRec(n1+n2,n2,n1+n2,i+1,n)
   else
      f
}
    
 def fibIter(ff:Int, n1n1:Int,n2n2:Int,ii:Int,nn:Int):Int = {
     var f = ff
     var n1 = n1n1
     var n2 = n2n2
     var i = ii
     var n = nn
    while (i <= n) {        
      f = n1 + n2
      n1 = n2
      n2 = f
      i = i + 1
    }
    f
  }

def main(args: Array[String]) {
    for (i <- 0 to 10)
      print(fib(i) + " ")

  }
}
