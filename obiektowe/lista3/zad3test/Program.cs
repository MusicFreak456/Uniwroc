using System;
using TimeNTon;

namespace test3
{
    class Program
    {
        static void Main()
        {
            Timenton a = Timenton.Instance();
            a.id = "Hiacynta";
            Console.WriteLine(a.id);
            Timenton b = Timenton.Instance();
            b.id = "Spycisław";
            Console.WriteLine(b.id);
            Timenton c = Timenton.Instance();
            c.id = "Krzysztof";
            Console.WriteLine(c.id);
            Timenton d = Timenton.Instance();
            d.id = "Brundengilda";
            Console.WriteLine(d.id);
            Timenton e = Timenton.Instance();
            e.id = "Mieszko";
            Console.WriteLine(e.id);
            Timenton f = Timenton.Instance();
            Console.WriteLine(f.id);
            Timenton g = Timenton.Instance();
            Console.WriteLine(g.id);
            
        }
    }
}