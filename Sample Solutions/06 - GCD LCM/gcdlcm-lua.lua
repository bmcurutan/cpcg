function gcdiv(a,b) 
    while (b ~= 0) do
        c = b
        b = a%b
        a = c
    end
    return a
end

function lcmul(a,b)
    return (a * b) / gcdiv(a,b);
end




