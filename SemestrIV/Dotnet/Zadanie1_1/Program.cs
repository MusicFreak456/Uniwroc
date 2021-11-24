using System;
using System.Collections.Generic;

namespace Zadanie1_1 {
    class Program {
        static void Main(string[] args) {
            Console.WriteLine(string.Join(", ", FindSet()));
        }

        public static List<int> FindSet() {
            List<int> result = new List<int>();

            for (int i = 1; i <= 100000; i++) {
                if (Validate(i)) result.Add(i);
            }

            return result;
        }

        public static bool Validate(int x) {
            List<int> digits = GetDigits(x);
            int sumOfDigits = GetSumOfDigits(digits);

            if (x % sumOfDigits != 0) return false;

            foreach (int digit in digits) {
                if (digit != 0 && 
                    x % digit != 0) 
                    return false;
            }

            return true;
        }

        public static List<int> GetDigits(int x) {
            List<int> result = new List<int>(6);
            int number = x;

            while (number != 0) {
                result.Add(number % 10);
                number /= 10;
            }

            return result;
        }

        public static int GetSumOfDigits(List<int> listOfDigits) {
            int sum = 0;

            foreach (int x in listOfDigits) {
                sum += x;
            }

            return sum;
        }
        
    }
}
