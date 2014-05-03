#include<stdio.h>
long[] quicksort(long[] items,long len);
long[] quicksortRec(long[] items,long leftIndex,long rightIndex);
void partition(long pivotIndex,long[] items,long leftIndex,long rightIndex);
long partitionIter(long swapIndex,long i,long rightIndex,long[] items,long pivotValue);
void swap(long[] items,long i,long j);
long[] quicksort(long[] items,long len) {
    if (len < 1) {
        return items;
    }
    else {
        return quicksortRec(items,0,len-1);
    }
}
long[] quicksortRec(long[] items,long leftIndex,long rightIndex) {
    if (leftIndex >= rightIndex) {
        return items;
    }
    else {
        long[] pivot = leftIndex;
        pivot = partition(pivot,items,leftIndex,rightIndex);
        quicksortRec(items,leftIndex,pivot - 1);
        quicksortRec(items,pivot + 1,rightIndex);
        return items;
    }
}
void partition(long pivotIndex,long[] items,long leftIndex,long rightIndex) {
    long pivotValue = items[pivotIndex];
    swap(items,pivotIndex,rightIndex);
    long swapIndex = partitionIter(leftIndex,leftIndex,rightIndex,items,pivotValue);
    swap(items,swapIndex,rightIndex);
    return swapIndex;
}
long partitionIter(long swapIndex,long i,long rightIndex,long[] items,long pivotValue) {
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
    return swapIndex;
}
void swap(long[] items,long i,long j) {
    long iVal = items[i];
    long jVal = items[j];
    items[i] = jVal;
    items[j] = iVal;
}


//**********LIST VERSION****************

#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
  
  struct List
{
  long val;
  struct List *next;
};

struct List* quicksortRec(struct List* items, long leftIndex, long rightIndex);
struct List* quicksortF(struct List* items, long len);
void partitionIter(struct List** less, struct List** rest, struct List* pivot, struct List* items);
void partitionRec (struct List** less, struct List** rest, struct List* pivot, struct List* items);

struct List* create_list(struct List *head, long val)
{
  printf("\n creating list with headnode as [%d]\n",val);
  struct List *ptr = (struct List*)malloc(sizeof(struct List));
  if(NULL == ptr)
  {
    printf("\n Node creation failed \n");
    return NULL;
  }
  ptr->val = val;
  ptr->next = NULL;
  
  head = ptr;
  return head;
}

struct List* end_of_list(struct List *head)
{
  struct List *ptr = head;
  struct List *prev = NULL;
  while(ptr != NULL)
  {
    prev = ptr;
    ptr = ptr->next;
  }
  return prev;
}


struct List* add_to_list(struct List *head,long val, bool add_to_end)
{
  struct List *curr = end_of_list(head);
  if(NULL == head)
  {
    return (create_list(head,val));
  }
  
  if(add_to_end)
    printf("\n Adding node to end of list with value [%d]\n",val);
  else
    printf("\n Adding node to beginning of list with value [%d]\n",val);
  
  struct List *ptr = (struct List*)malloc(sizeof(struct List));
  if(NULL == ptr)
  {
    printf("\n Node creation failed \n");
    return NULL;
  }
  ptr->val = val;
  ptr->next = NULL;
  
  if(add_to_end)
  {
    curr->next = ptr;
    curr = ptr;
  }
  else
  {
    ptr->next = head;
    head = ptr;
  }
  return head;
}

struct List* search_in_list(struct List *head,long val, struct List **prev)
{
  struct List *ptr = head;
  struct List *tmp = NULL;
  bool found = false;
  
  printf("\n Searching the list for value [%d] \n",val);
  
  while(ptr != NULL)
  {
    if(ptr->val == val)
    {
      found = true;
      break;
    }
    else
    {
      tmp = ptr;
      ptr = ptr->next;
    }
  }
  
  if(true == found)
  {
    if(prev)
      *prev = tmp;
    return ptr;
  }
  else
  {
    return NULL;
  }
}

long length(struct List *tlist)
{
  struct List *ptr = tlist;
  long count = 0;
  while(ptr != NULL)
  {
    count = count + 1;
    ptr = ptr->next;
  }
  return count;
}

struct List* delete_from_list(struct List *head,long val)
{
  struct List *curr = end_of_list(head);
  struct List *prev = NULL;
  struct List *del = NULL;
  
  printf("\n Deleting value [%d] from list\n",val);
  
  del = search_in_list(head,val,&prev);
  if(del == NULL)
  {
    return head;
  }
  else
  {
    if(prev != NULL)
      prev->next = del->next;
    
    if(del == curr)
    {
      curr = prev;
    }
    else if(del == head)
    {
      head = del->next;
    }
  }
  
  free(del);
  del = NULL;
  
  return head;
}

struct List* concat(struct List* l1, struct List* l2)
{
  long x = length(l1);
  long y = length(l2);
  
  if (x >0 && y > 0)
  {
    end_of_list(l1)->next = l2;
  }
  if(x == 0)
  {
    return l2;
  }
  
  return l1; 
}



void print_list(struct List *head)
{
  struct List *ptr = head;
  
  printf("\n -------Printing list Start------- \n");
  while(ptr != NULL)
  {
    printf("\n [%d] \n",ptr->val);
    ptr = ptr->next;
  }
  printf("\n -------Printing list End------- \n");
  
  return;
}

void partition(struct List** less, struct List** rest, struct List* pivot, struct List* items) 
{
  // partitionIter(less,rest,pivot,items);
  partitionRec(less,rest,pivot,items);
}

void partitionIter(struct List** less, struct List** rest, struct List* pivot, struct List* items) 
{
  while (!(items == NULL) && length(items) >= 1) {
    struct List *x = items;
    items = items->next;
    if (x->val < pivot->val) {
      x->next = *less;
      *less = x;
    } 
    else {
      x->next = *rest;
      *rest = x;
    }
  }
}

void partitionRec(struct List** less, struct List** rest, struct List* pivot, struct List* items) 
{
  if (!(items == NULL) && length(items)>= 1)
  {
    struct List *x = items;
    items = items->next;
    partitionRec(less,rest,pivot,items);
    if (x->val < pivot->val) {
      x->next = *less;
      *less = x;
      rest = rest;
    } 
    else {
      less=less;
      x->next = *rest;
      *rest = x;
    }
  }
}

struct List* quicksortRec(struct List* items, long leftIndex, long rightIndex)
{
  if (leftIndex >= rightIndex) 
  {
    return items;
  }
  else 
  { 
    struct List *pivot = items;
    struct List *less = NULL;
    struct List *rest = NULL;
    items = items->next;
    
    partition(&less,&rest,pivot,items);
    pivot->next = quicksortRec(rest,0,length(rest)-1);
    return concat(quicksortRec(less,0,length(less)-1),pivot);
  }
} 

struct List* quicksortF(struct List* items, long len) 
{
  if (len < 1) 
  {
    return items;
    
  }
  else 
  {
    return quicksortRec(items,0,len-1);
  }
}


long main(void)
{
  struct List *ptr = NULL;
  struct List *ptr2 = NULL;
  
  
  ptr = add_to_list(ptr,3,false);
  ptr = add_to_list(ptr,2,false);
  ptr = add_to_list(ptr,5,false);
  ptr = add_to_list(ptr,2,false);
  ptr = add_to_list(ptr,1,false);
  ptr = add_to_list(ptr,4,false);
  ptr = add_to_list(ptr,-1,false);
  print_list(ptr);
  ptr = quicksortF(ptr, length(ptr));
  print_list(ptr);
  printf("TEST2");
  return 0;
}
