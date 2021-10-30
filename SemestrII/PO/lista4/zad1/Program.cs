using System;

namespace zad1
{
    class Program
    {
        static void Main(string[] args)
        {
            Lista<int> nowa = new Lista<int>();
            Console.WriteLine(nowa);
            Console.WriteLine(nowa.Length);
            nowa.addBack(1);
            nowa.addBack(2);
            nowa.addFront(0);
            nowa.addBack(3);
            nowa.addBack(4);
            Console.WriteLine(nowa);
            Console.WriteLine("Popped front: {0}",nowa.popFront());
            Console.WriteLine("Popped back: {0}",nowa.popBack());
            Console.WriteLine(nowa);
            Console.WriteLine("Length: {0}", nowa.Length);
            Console.WriteLine("index=1: {0}", nowa[1]);
            Console.WriteLine("foreach: ");
            foreach (int item in nowa)
            {
                Console.WriteLine(item);
            }
        }
    }
}
