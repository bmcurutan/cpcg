long gcdiv(long a, long b);

     long gcdiv(long a, long b) {
        while (b!= 0) {
            long c = a % b;
            a = b;
            b = c;
        }
        return a;
    }

    long lcmul(long a, long b) {
        return (a*b)/gcdiv(a,b);
    }

