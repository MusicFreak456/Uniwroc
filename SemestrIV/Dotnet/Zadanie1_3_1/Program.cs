using System;
using System.Text.RegularExpressions;
using System.Linq;

namespace Zadanie1_3_1 {
    
    public static class StringExtension {
        public static bool isPalindrome(this string x) {
            string trimmedAndLowered = Regex.Replace(x.ToLower(), @"[^\w]", "");
            string reversed = new string(trimmedAndLowered.Reverse().ToArray());
            return trimmedAndLowered == reversed;
        }
    }
    
    class Program {
        static void Main(string[] args) {
            string s = "Kobyła ma mały bok.";
            Console.WriteLine(s.isPalindrome());
            s = "12344321";
            Console.WriteLine(s.isPalindrome());
            s = "   ,.  1234,4321   ,!?   ";
            Console.WriteLine(s.isPalindrome());
            s = "To nie jest palindrom";
            Console.WriteLine(s.isPalindrome());
        }
    }
}
