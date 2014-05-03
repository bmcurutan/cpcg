function quicksort(items,len)
    if (len < 1) then
        return items
    else
        return qsRec(items,1,len)
    end
end

function tail(t)
  local function helper(head, ...) return #{...} > 0 and {...} or nil end
  return helper((table.unpack or unpack)(t))
end

function qsRec(items,leftIndex,rightIndex)
    if (leftIndex >= rightIndex) then
        return items
    else
        local pivot = items[1]
        local less = {}
        local rest = {}
        table.remove(items,1)
        partition(less,rest,pivot,items)
        return concat(qsRec(less,1,#less), listConcat(pivot,qsRec(rest,1,#rest)))
    end
end

function partition(less,rest,pivot,items)
    --partRec(less,rest,pivot,items)
partIter(less,rest,pivot,items)
end

function partRec(less,rest,pivot,items)
    if (items ~= nil and #items >= 1) then
        local x = items[1]
        table.remove(items,1)
        partRec(less,rest,pivot,items)
        if (x < pivot) then
            less = listConcat(x,less)
        else
            rest = listConcat(x,rest)
        end
    end
end

function listConcat(nd,l)
    table.insert(l,1,nd)
    return l
end

function concat(l1,l2)
    if (l2 == nil or #l2 < 1) then
        return l1
    else
        table.insert(l1,l2[1])
        table.remove(l2,1)
        return concat(l1,l2)
    end
end


function partIter(less,rest,pivot,items)
    while (items ~= nil and #items >= 1) do
        local x = items[1]
        table.remove(items,1)
        if (x < pivot) then
            less = listConcat(x,less)
        else
            rest = listConcat(x,rest)
        end
    end
end 

--[[function quicksort(items,len)
    if (len < 1) then
        return items
    else 
        return qsRec(items,1,len)
    end
end

function qsRec(items,leftIndex,rightIndex)
    if (leftIndex >= rightIndex) then
        return items
    else
        local pivot = leftIndex
        pivot = partition(pivot,items,leftIndex,rightIndex)
        qsRec(items,leftIndex,pivot-1)
        qsRec(items,pivot+1,rightIndex)
        return items
    end
end

function partition(pivotIndex,items,leftIndex,rightIndex)
    local pivotValue = items[pivotIndex]
    swap(items,pivotIndex,rightIndex)
    local swapIndex = partRec(leftIndex,leftIndex,rightIndex,items,pivotValue)
    swap(items,swapIndex,rightIndex)
    return swapIndex
end

function partRec(swapIndex,i,rightIndex,items,pivotValue)
    if (i < rightIndex) then
        local iVal = items[i]
        if (iVal < pivotValue) then
            swap(items,i,swapIndex)
            return partRec(swapIndex+1,i+1,rightIndex,items,pivotValue)
        else
            return partRec(swapIndex,i+1,rightIndex,items,pivotValue)
        end
    else
        return swapIndex
    end
end

function swap(items,i,j)
    local iVal = items[i]
    local jVal = items[j]
    items[i] = jVal
    items[j] = iVal
end]]

print(unpack({5, 2, 7, 3, 4, 7, 1}))
print(unpack(quicksort({5, 2, 7, 3, 4, 7, 1},7)))