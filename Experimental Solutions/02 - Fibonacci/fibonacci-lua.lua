function fibonacci(n) 
    if (n < 2) then
        return n
    else 
        --return fibRec(n,0,1,2,n)
        return fibIter(n,0,1,2,n)
    end
end

function fibRec(f,n1,n2,i,n)
    if (i <= n) then
        return fibRec(n1+n2,n2,n1+n2,i+1,n)
    else
        return f
    end
end

function fibIter(f,n1,n2,i,n)
    while (i <= n) do
        f = n1 + n2
        n1 = n2
        n2 = f
        i = i + 1
    end
    return f
end

print(fibonacci(0))
print(fibonacci(1))
print(fibonacci(2))
print(fibonacci(3))
print(fibonacci(4))
print(fibonacci(5))
print(fibonacci(6))