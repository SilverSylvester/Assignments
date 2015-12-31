using System;

namespace Solution {
    class Solution {
        static void Main(string[] args) {
            long a = long.Parse(Console.ReadLine());
            long b = long.Parse(Console.ReadLine());

            Console.WriteLine(gcd(a,b));
        }

        static long gcd(long a, long b) {
            if (b == 0) return a;
            else return gcd(b, a % b);
        }
    }
}

