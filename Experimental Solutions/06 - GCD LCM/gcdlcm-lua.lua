function gcdiv(a,b) 
    return gcdRec(a,a,b)
   --return gcdIter(a,a,b)
end

function gcdRec(g,a,b)
    if (b ~= 0) then
        return gcdRec(b,b,a%b)
    else
        return a
    end
end

function gcdIter(g,a,b)
    while (b ~= 0) do
        g = b
        b = a%b
        a = g
    end
    return a
end

function lcmul(a,b)
    y,z = a * b,gcdiv(a,b);
    return y / z;
end

print(gcdiv(16,48))
print(lcmul(3,6))



