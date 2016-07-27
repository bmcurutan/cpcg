using System;
using System.Linq;

class Program
{
        public static bool Palindrome(String a)
        {
            string b = new string(a.Reverse().ToArray());
            Console.WriteLine("** " + b);
            return a.Equals(b);
        }

    static void Main()
    {
	Console.WriteLine(Palindrome("racecar"));
		Console.WriteLine(Palindrome("bianca"));
}
}