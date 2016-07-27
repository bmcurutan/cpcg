function exponent(a,b) 
    return expRec(1,a,b)
    --return expIter(1,a,b)
end

function expRec(e,a,b)
    if (b ~= 0) then
        expRec(e*a,a,b-1)
    else
        return e
    end
end

function expIter(e,a,b)
    while (b ~= 0) do
        e = e*a
        b = b-1
    end
    return e
end

print(exponent(2,4))