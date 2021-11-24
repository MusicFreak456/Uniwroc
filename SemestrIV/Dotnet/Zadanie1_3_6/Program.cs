using System;
using System.Linq;
using System.IO;

namespace Zadanie1_3_6 {
    class Program {
        static void Main(string[] args) {
            string[] logs = File.ReadAllLines("./log.txt");

            var mostActiveIPs = logs
                .Select(x => x.Split(' '))
                .GroupBy(x => x[1])
                .OrderByDescending(x => x.Count())
                .Select(x => x.Key.ToString() + " " + x.Count())
                .Take(3);

            foreach(var IP in mostActiveIPs) {
                Console.WriteLine(IP);
            }
        }
    }
}
