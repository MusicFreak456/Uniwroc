using System;
using System.Collections.Generic;

namespace Zadanie1_2_3 {
    class Program {
        
        static void prettyPrint(string text, List<int> list) {
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine(text);
            foreach (int x in list) {
                Console.Write(x + " ");
            }
            Console.Write("\n");
        }

        static void Main(string[] args) {
            List<int> Example = new List<int>() { 4, 2, 2, 1, 3, 7, 1, 4, 1, 0 };

            Console.WriteLine("ForEach:");
            Example.ForEach(i => Console.Write(i + " "));
            Console.Write("\n");

            prettyPrint("ConvertAll: *2 ", Example.ConvertAll(x => x * 2));
            prettyPrint("FindAll: nieparzyste", Example.FindAll(x => Convert.ToBoolean(x % 2)));
            Example.RemoveAll(x => x < 2);
            prettyPrint("RemoveAll: <2", Example);
            Example.Sort( (a,b) => {
                if (a > b) return -1;
                else if (a != b) return 1;
                return 0;
            });
            prettyPrint("Sort Desc", Example);
        }
    }
}
