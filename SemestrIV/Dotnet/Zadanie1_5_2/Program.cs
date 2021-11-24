using System;
using System.Text;
using System.Runtime.InteropServices;

namespace Zadanie1_5_2 {
    class Program {
        [DllImport("Zadanie1_5_2_dll.dll")]
        private static extern int IsPrimeC(int n);

        static void Main(string[] args) {
            string nStr = Console.ReadLine();
            int n = int.Parse(nStr);
            Console.WriteLine("Wynik IsPrimeC: " + IsPrimeC(n).ToString());
        }
    }
}
