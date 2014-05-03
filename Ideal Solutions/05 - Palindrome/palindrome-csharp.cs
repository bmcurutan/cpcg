using System;
using System.Linq;

class Program
{
        public static bool Palindrome(String a)
        {
            return a.Equals(new string(a.Reverse().ToArray()));
        }
}