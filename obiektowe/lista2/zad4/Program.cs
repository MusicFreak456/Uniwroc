using System;

namespace zad4
{
    class Program
    {
        static void Main(string[] args)
        {
            ListaLeniwa nowaLista = new ListaLeniwa();
            Pierwsze nowaListaPierwsze = new Pierwsze();
            Console.WriteLine("Size: {0}",nowaLista.size());
            Console.WriteLine("40th element: {0}",nowaLista.element(40));
            Console.WriteLine("Size: {0}",nowaLista.size());
            Console.WriteLine("38th element: {0}",nowaLista.element(38));
            Console.WriteLine("Size: {0}",nowaLista.size());
            Console.WriteLine("");
            Console.WriteLine("Size: {0}",nowaListaPierwsze.size());
            Console.WriteLine("1st Element: {0}",nowaListaPierwsze.element(1));
            Console.WriteLine("Size: {0}",nowaListaPierwsze.size());
            Console.WriteLine("3rd Element: {0}",nowaListaPierwsze.element(3));
            Console.WriteLine("Size: {0}",nowaListaPierwsze.size());
            Console.WriteLine("2nd Element: {0}",nowaListaPierwsze.element(2));
            Console.WriteLine("Size: {0}",nowaListaPierwsze.size());

        }
    }
}
