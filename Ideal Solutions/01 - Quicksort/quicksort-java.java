public class Quicksort {

  public void quicksort(int[] items) {
    if (items.length < 1) {
      return;
    }
    quicksortRec(items, 0, items.length-1);
  }
   
  private void quicksortRec(int[] items, int leftIndex, int rightIndex) {
    if (leftIndex < rightIndex) {
      int pivotIndex = partition(items, leftIndex, rightIndex);
      quicksortRec(items, leftIndex, pivotIndex-1);
      quicksortRec(items, pivotIndex+1, rightIndex);
    }
  }
   	 
  private int partition(int[] items, int leftIndex, int rightIndex) {
    int pivotValue = items[rightIndex];
    int i = leftIndex-1;
    int j = leftIndex;   

    while (j < rightIndex) {
      if(items[j] < pivotValue) {
        i++;
        swap(items, i, j);
      }
      j++;
    }

    swap(items, i+1, rightIndex);
    return i+1;
  }
   	
  private void swap(int[] items, int i, int j) {
    int k = items[i];
    items[i] = items[j];
    items[j] = k;
  } 
} 