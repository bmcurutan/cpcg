import java.util.*;

public class Test {    
  public Integer[] quicksort(Integer[] items, int len) {
    if (len < 1) {
      return items;
    }
    else {
      return quicksortRec(items, 0, len-1);
    }
  }
  
  private Integer[] quicksortRec(Integer[] items, int leftIndex, int rightIndex) {
    if (leftIndex >= rightIndex) {
      return items;
    }
    else {      
      int pivot = leftIndex;  //choose first element as pivot
      pivot = partition(pivot, items, leftIndex, rightIndex);
      quicksortRec(items, leftIndex, pivot-1);
      quicksortRec(items, pivot+1, rightIndex); 
      return items; 
    }
  }
  
  private int partition(int pivotIndex, Integer[] items, int leftIndex, int rightIndex) {
    int pivotValue = items[pivotIndex];
    swap(items,pivotIndex,rightIndex);  // Move pivot to end
    
    //iterative
    // int swapIndex = partitionIter(leftIndex, leftIndex, rightIndex, items, pivotValue);
    
    //recursive
    int swapIndex = partitionRec(leftIndex, leftIndex, rightIndex, items, pivotValue);
    
    swap(items,swapIndex,rightIndex);  // Move pivot to its final place
    return swapIndex;
  }
  
  private int partitionIter(int swapIndex, int i, int rightIndex, Integer[] items, int pivotValue) {
    while (i < rightIndex) {
      int iVal = items[i];
      if (iVal < pivotValue) {
        swap(items,i,swapIndex);
        swapIndex = swapIndex+1;
		i=i+1;
      }
      else { 
        i = i + 1; 
      }
    } 
    return swapIndex;
  }
  
  private int partitionRec(int swapIndex, int i, int rightIndex, Integer[] items, int pivotValue) {
    if (i < rightIndex) {
      int iVal = items[i];
      if (iVal < pivotValue) {
        swap(items, i, swapIndex);
        return partitionRec(swapIndex+1, i+1, rightIndex, items, pivotValue);
      }
      else {
        return partitionRec(swapIndex, i+1, rightIndex, items, pivotValue);
      }     
    }
    else {
      return swapIndex;
    }
  }
  
  private void swap(Integer[] items, int i, int j) {
    int iVal = items[i];
    int jVal = items[j];
    items[i] = jVal;
    items[j] = iVal;
  }
  
  public static void main(String[] args) {   
    Test qs = new Test();
    // System.out.println(qs.recFac(4) + " " + qs.iterFac(4));
    
    Integer[] test1 = {2,5,0,4,1,-10,3,4};
    
    System.out.println("Array:");
    print(test1);
    test1 = qs.quicksort(test1,test1.length);
    print(test1);
    
    /*---------------------------------*/
    
    System.out.println();
    
    List<Integer> test2 = new LinkedList<Integer>();
    test2.add(2);
    test2.add(5);
    test2.add(0);
    test2.add(4);
    test2.add(1); 
    test2.add(-10); 
    test2.add(3); 
    test2.add(4);
    
    System.out.println("List<Integer>:");
    printF(test2);
    test2 = qs.quicksortF(test2, test2.size());
    printF(test2);
    
    //List<Integer> test3 = new List<Integer>();
    //qs.tail(test3);
  }
  
  public static void print(Integer[] a) {
    if (a != null) {
      for (int i = 0; i < a.length; i++) {
        System.out.print(a[i] + " ");
      }
    }
    System.out.println();
  }
  
  public static void printF(List<Integer> l) {
    if (l != null) {
      for (Integer o: l) {
        System.out.print(o + " ");
      }
    }
    System.out.println();
  }
  
  /* ----- Quicksort Immutable List<Integer> ("Functional") ----- */ 
  public List<Integer> quicksortF(List<Integer> items, int len) {
    if (len < 1) {
      return items;
    }
    
    else {
      return qsRec(items, 0, len-1);
    }
  } 
  
  public List<Integer> qsRec(List<Integer> items, int leftIndex, int rightIndex) {
    if (leftIndex >= rightIndex) {
      return items;
    }
    else {
      int pivot = items.get(0); 
      List<Integer> less = new LinkedList<Integer>();
      List<Integer> rest = new LinkedList<Integer>();
      items.remove(0);
      partition(less, rest, pivot, items);
      
      return concat(qsRec(less,0,less.size()-1),list(pivot,qsRec(rest,0,rest.size()-1)));
    }
  }
  
  public List<Integer> list(int nd, List<Integer> l) {
    l.add(0,nd); //add nd to head of l
    return l;
  }
  
  public List<Integer> concat(List<Integer> l1, List<Integer> l2) {
    if (l2.size() < 1) {
      return l1;
    }
    else {
      l1.add(l2.get(0));
      l2.remove(0);
      return concat(l1,l2);
    }   
  }
  
  public void partition(List<Integer> less, List<Integer> rest, int pivot, List<Integer> items) {
    partitionIter(less,rest,pivot,items);
    // partitionRec(less,rest,pivot,items);
  }
  
  private void partitionIter(List<Integer> less, List<Integer> rest, int pivot, List<Integer> items) {
      while (items.size() >= 1) {
        int x = items.get(0);
        items.remove(0);
        if (x < pivot) {
          less = list(x,less);
          rest = rest;
        }
        else {
          less = less;
          rest = list(x,rest);
        }
      }
    }
    
  private void partitionRec(List<Integer> less, List<Integer> rest, int pivot, List<Integer> items) {
    if (items.size() >= 1) {
      int x = items.get(0);
      items.remove(0);
      partitionRec(less, rest, pivot, items);
      if (x < pivot) {
        less = list(x,less);
        rest = rest;
      }
      else {
        less = less;
        rest = list(x,rest);
      }
    }
  }
  
  
  /*-----------------------------------------------------------*/
  public static void printArr(int[] a) {
    for (int i = 0; i < a.length; i++) {
      System.out.print(a[i] + " ");
    }
    System.out.println();
  }
  
}


