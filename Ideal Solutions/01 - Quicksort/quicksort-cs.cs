using System;
using System.Collections.Generic;

class QuicksortProg {

  public static void Quicksort(int[] items) {
    if (items.Length < 1) {
      return;
    }
    QuicksortRec(items, 0, items.Length-1);
  }
   
  private static void QuicksortRec(int[] items, int leftIndex, int rightIndex) {
    if (leftIndex < rightIndex) {
      int pivot = Partition(items, leftIndex, rightIndex);
      QuicksortRec(items, leftIndex, pivot-1);
      QuicksortRec(items, pivot+1, rightIndex);
    }
  }
        
  private static int Partition(int[] items, int leftIndex, int rightIndex) {
    int pivot = items[rightIndex];
    int i = leftIndex-1;
    int j = leftIndex;   

    while (j < rightIndex) {
      if(items[j] < pivot) {
        i++;
        Swap(items, i, j);
      }
      j++;
    }

    Swap(items, i+1, rightIndex);
    return i+1;
  }
   	
  private static void Swap(int[] items, int i, int j) {
    int k = items[i];
    items[i] = items[j];
    items[j] = k;
  } 
}
  