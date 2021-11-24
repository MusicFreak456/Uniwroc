using System;
using System.Collections.Generic;

namespace Zadanie1_4_1 {

    class Program {
        
        static int RegularFoo(int x, int y) {
            return (x - y) * 2 * y;
        }

        static dynamic DynamicFoo(dynamic x, dynamic y) {
            return (x - y) * 2 * y;
        }

        static TimeSpan Test(string mode, Random randomNumberGenerator) {
            int test = 0;
            DateTime start = DateTime.Now;

            if (mode == "regular") {
                for (int i = 1; i <= 1000000; i++) {
                    test += RegularFoo(randomNumberGenerator.Next(42), randomNumberGenerator.Next(42));
                }
            }
            else {
                for (int i = 1; i <= 1000000; i++) {
                    test += DynamicFoo(randomNumberGenerator.Next(42), randomNumberGenerator.Next(42));
                }
            }

            DateTime end = DateTime.Now;
            Console.Write(test);

            return end - start;
        }

        static void Compare() {
            Random random = new Random();

            Console.WriteLine("Wywołania na rozruch:");
            for (int i = 1; i <= 5; i++) {
                Test("regular", random);
                Test("dynamic", random);
            }

            Console.Write("\n");
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Początek porównania:");

            List<Tuple<double, double>> results = new List<Tuple<double, double>>();
            for (int i = 1; i <= 5; i++) {
                double result_regular = Test("regular", random).TotalMilliseconds;
                double result_dynamic = Test("dynamic", random).TotalMilliseconds;
                Tuple<double, double> result = new Tuple<double, double>(result_regular, result_dynamic);
                results.Add(result);
            }
            double average = 0;

            foreach (Tuple<double, double> result in results) {
                average += 1 - (result.Item1 / result.Item2);
            }

            int percent = (int)((average / results.Count) * 100);

            Console.Write("\n");
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Kod z typowaniem dynamicznym wolniejszy o średnio: " + (percent.ToString() + "%"));
        }

        static void Main(string[] args) {
            Compare();
        }
    }
}
