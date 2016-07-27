#include<stdio.h>

void quicksort(long items[],long len);
void quicksortRec(long items[],long leftIndex,long rightIndex);
long partition(long pivotIndex,long items[],long leftIndex,long rightIndex);
void swap(long items[],long i,long j);

void quicksort(long items[],long len) {
    if (len < 1) {
        return;
    }
    else {
        quicksortRec(items,0,len-1);
    }
}

void quicksortRec(long items[],long leftIndex,long rightIndex) {
    if (leftIndex >= rightIndex) {
        return;
    }
    else {
        long pivot = leftIndex;
        pivot = partition(pivot,items,leftIndex,rightIndex);
        quicksortRec(items,leftIndex,pivot - 1);
        quicksortRec(items,pivot + 1,rightIndex);
    }
}

long partition(long pivotIndex,long items[],long leftIndex,long rightIndex) {
    long pivotValue = items[pivotIndex];
    swap(items,pivotIndex,rightIndex);
    long swapIndex,i;
    while (i < rightIndex) {
        long iVal = items[i];
        if (iVal < pivotValue) {
            swap(items,i,swapIndex);
            swapIndex = swapIndex + 1;
            i = i + 1;
        }
        else {
            i = i + 1;
        }
    }
    swap(items,swapIndex,rightIndex);
    return swapIndex;
}

void swap(long items[],long i,long j) {
    long temp = items[i];
    items[i] = items[j];
    items[j] = temp;
}




