using System;
using System.Collections.Generic;

class Pierwsze :ListaLeniwa
{
    private PrimeStream primStream;

    public Pierwsze()
    {
        lista = new List<int>();
        primStream = new PrimeStream();
    }

    public override int element(int i)
    {
        if(i<=size())
        {
            return lista[i-1];
        }
        else
        {
            while(i>size())
            {
                lista.Add(primStream.next());
            }
            return lista[i-1];
        }
    }



}
