function factorial(i) 
    --return facRec(1,i)
    return facIter(1,i)
end

function facRec(f,i)
    if (i > 0) then
        return facRec(f*i,i-1)
    else
        return f
    end
end

function facIter(f,i)
    while (i > 0) do
        f = f*i
        i = i-1
    end
    return f
end

print(factorial(0))
print(factorial(1))
print(factorial(2))
print(factorial(3))
print(factorial(4))
print(factorial(5))
print(factorial(6))