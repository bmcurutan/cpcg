function quicksort(items)
  if (#items < 1) then
    return 
  else
    return qsRec(items,1,#items)
  end
end
   	
function qsRec(items,leftIndex,rightIndex)
  if (leftIndex < rightIndex) then       
    local pivotIndex = partition(items,leftIndex,rightIndex)         
    qsRec(items,leftIndex,pivotIndex-1)         
    qsRec(items,pivotIndex+1,rightIndex)         
    return items
  end 
end
   	
function partition(items,leftIndex,rightIndex)     
  local pivotValue = items[rightIndex]     
  local i = leftIndex-1     
  local j = leftIndex     

  while (j < rightIndex) do     
    if (items[j] < pivotValue) then     
      i = i+1         
      swap(items,i,j)       
    end       
    j = j+1
  end

  swap(items,i+1,rightIndex)     
  return i+1 
end
   	
function swap(items,i,j)     
  local k = items[i]     
  items[i] = items[j] 
  items[j] = k 
end