using System;
using Slownik;

namespace test
{
    class Program
    {
        static void Main(string[] args)
        {
            Slownik<int,string> nowy = new Slownik<int,string>();
            nowy.add(1,"Pierwszy");
            nowy.add(2,"Drugi");
            nowy.add(4,"Czwarty");
            nowy.add(4,"Powtórzony");

            
            nowy.display();
            Console.WriteLine("\n");
            nowy.remove(2);
            nowy.display();
            Console.WriteLine("\n");
            nowy.remove(1);
            nowy.display();
            Console.WriteLine("\n");
            nowy.remove(4);
            nowy.display();
        }
    }
}
