using System;
using System.Linq;
using System.IO;

namespace Zadanie1_3_5 {
    class Program {
        static void Main(string[] args) {
            string[] personalDataSet = File.ReadAllLines("./dane_osobowe.txt");
            string[] accounts = File.ReadAllLines("./konta.txt");

            Console.WriteLine("Dane osobowe:");
            foreach(var personal_data in personalDataSet) {
                Console.WriteLine(personal_data);
            }

            Console.WriteLine("\nDane konta:");
            foreach(var account in accounts) {
                Console.WriteLine(account);
            }

            var personalDataSetSplitted = personalDataSet.Select(x => x.Split(' ')); //Załóżmy, że dane oddzielone są spacją
            var accountsSplitted = accounts.Select(x => x.Split(' '));

            var joined_data = from personalData in personalDataSetSplitted
                              join account in accountsSplitted on personalData[2] equals account[0]
                              select personalData[0] + " " +
                                     personalData[1] + " " +
                                     personalData[2] + " " +
                                     account[1];

            Console.WriteLine("\nDane połączone:");
            foreach (var data in joined_data) {
                Console.WriteLine(data);
            }
        }
    }
}
