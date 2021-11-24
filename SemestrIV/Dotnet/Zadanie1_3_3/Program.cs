using System;
using System.Linq;
using System.IO;

namespace Zadanie1_3_2 {
    class Program {
        static void Main(string[] args) {
            string[] surnames = File.ReadAllLines("./nazwiska.txt");

            var firstLetters = from surname in surnames
                               group surname by surname[0] into firstLettersGroups
                               orderby firstLettersGroups.Key
                               select firstLettersGroups.Key;

            foreach(var letter in firstLetters) {
                Console.Write(letter + " ");
            }

        }
    }
}
