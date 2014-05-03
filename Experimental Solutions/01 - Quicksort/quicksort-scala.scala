object Quicksort {
  
  /* ----- oo ----- */
  def quicksortOO(items:Array[Int], len:Int): Array[Int] = {
   if (len < 1) {
   items
   }
   else {
   quicksortRec(items,0,len-1)
   }
   items
   }
   
   def quicksortRec(items:Array[Int], leftIndex:Int, rightIndex:Int): Unit = {
   if (leftIndex >= rightIndex) {
   items
   }
   else {
   var pivot:Int = leftIndex
   pivot = partition(pivot,items, leftIndex, rightIndex)
   quicksortRec(items, leftIndex, pivot-1)
   quicksortRec(items, pivot+1, rightIndex)
   items
   }
   }
   
   def partition(pivotIndex:Int, items:Array[Int], leftIndex:Int, rightIndex:Int): Int = {
   var pivotValue:Int = items(pivotIndex)
   swap(items,pivotIndex,rightIndex)
   
   //iterative
   var swapIndex:Int = partitionIter(leftIndex,leftIndex,rightIndex,items,pivotValue)
   
   //recursive
   //var swapIndex:Int = partitionRec(leftIndex,leftIndex,rightIndex,items,pivotValue)
   
   swap(items,swapIndex,rightIndex)
   swapIndex
   }
   
    def partitionIter( swapIndexswapIndex:Int, ii:Int, rightIndexrightIndex:Int, itemsitems:Array[Int], pivotValuepivotValue:Int):Int = {
   var swapIndex = swapIndexswapIndex
var i = ii
var rightIndex = rightIndexrightIndex
var items = itemsitems
var pivotValue = pivotValuepivotValue
    while (i < rightIndex) {
      var iVal:Int = items(i)
      if (iVal < pivotValue) {
        swap(items,i,swapIndex)
        swapIndex = swapIndex+1
i = i+1
      }
      else { 
        i = i + 1
      }
    } 
     swapIndex
  }
   
   def partitionRec( swapIndex:Int,  i:Int,  rightIndex:Int, items:Array[Int], pivotValue:Int):Int ={
   if (i < rightIndex) {
   var iVal:Int = items(i)
   if (iVal < pivotValue) {
   swap(items, i, swapIndex)
   partitionRec(swapIndex+1, i+1, rightIndex, items, pivotValue)
   }
   else {
   partitionRec(swapIndex, i+1, rightIndex, items, pivotValue)
   }     
   }
   else {
   swapIndex
   }
   }
   
   def swap( items:Array[Int],  i:Int, j:Int):Unit ={
   var iVal:Int = items(i)
   var jVal:Int = items(j)
   items(i) = jVal
   items(j) = iVal
   }
  
  
  def main(args: Array[String]) {
    println("Test")
      /*var a = List(5,4,3,3,2,-1)
      a.foreach(print)
      a = quicksortF(a,6)
      a.foreach(print)*/
      
      var a = Array(5,-4,3,1)
       for(i <- 0 until a.length){
       print(a(i) + " ")
       }
       a = quicksortOO(a,4)
          for(i <- 0 until a.length){
       print(a(i) + " ")
       }
  }
  
  /* ----- functional ----- */ 
  /*def quicksortF(items:List[Int],len:Int):List[Int] = {
    var len:Int = items.length
    if (len < 1) {
      items
    }
    else {
      qsRec(items,0,len-1)
    }
  }
  
  def qsRec(items:List[Int],leftIndex:Int,rightIndex:Int):List[Int] = {
    if (leftIndex >= rightIndex) {
      items 
    }
    else {
      var pivot:Int = items.head
        //var (less, rest) = items.tail partition (_ < pivot)
        var (less,rest) = partition(pivot,items.tail)
      List.concat(quicksortF(less,less.length)  , pivot :: quicksortF(rest,rest.length))
    }
  }
  
*/
}