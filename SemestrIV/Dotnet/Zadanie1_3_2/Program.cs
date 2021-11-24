using System;
using System.Linq;
using System.IO;

namespace Zadanie1_3_2 {
    class Program {
        static void Main(string[] args) {
            string[] lines = File.ReadAllLines("./liczby.txt");

            var numbers = from line in lines
                          let number = int.Parse(line)
                          where number > 100
                          orderby number descending
                          select number;

            Console.Write("Wyrażenie LINQ:\n");
            foreach(var number in numbers) {
                Console.Write(number.ToString() + " ");
            }

            numbers = lines
                    .Select(x => int.Parse(x))
                    .Where(x => x > 100)
                    .OrderByDescending(x => x);

            Console.Write("\nCiąg wywołań metod LINQ:\n");
            foreach (var number in numbers) {
                Console.Write(number.ToString() + " ");
            }

            //Parametrami OrderBy i Where są delegaty
            //Parametrami orderby i where są wyrażenia,
            //lub opcjonalne modyfikatory
        }
    }
}
