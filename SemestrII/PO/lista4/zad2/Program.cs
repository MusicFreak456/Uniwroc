using System;

namespace zad2
{
    class Program
    {
        static void Main(string[] args)
        {
            PrimeCollection pc = new PrimeCollection();
            foreach(int p in pc)
            {
                Console.WriteLine(p);
            }
        }
    }
}
