using System;
using System.Runtime.InteropServices;

namespace Zadanie1_5_3 {
    class Program {
        
        // Użycie funkcji z poprzedniego zadania (jeśli o to chodziło, nie jestem
        // pewien czy rozumiem pytanie), jeśli chodzi o wprost przekopiowanie
        // kodu napisanego w C do C#, to jest to możliwe, ale nie widzę do tego
        // powodu, kiedy mamy go już w jednym miejscu.
        [DllImport("Zadanie1_5_2_dll.dll")]
        private static extern int IsPrimeC(int n);

        static int IsPrimeCs(int n) {
            return IsPrimeC(n);
        }

        // na podstawie https://stackoverflow.com/questions/43226928/how-to-pass-function-pointer-from-c-sharp-to-a-c-dll
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        delegate int Del(int n);

        [DllImport("Zadanie1_5_3_dll.dll")]
        private static extern int ExecuteC(int n,Del f);

        static void Main(string[] args) {
            string nStr = Console.ReadLine();
            int n = int.Parse(nStr);
            Console.WriteLine("Wynik ExecuteC: " + ExecuteC(n, IsPrimeCs).ToString());
        }
    }
}
