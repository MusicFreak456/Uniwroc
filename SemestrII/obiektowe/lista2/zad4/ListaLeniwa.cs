using System;
using System.Collections.Generic;

class ListaLeniwa
{
    protected List<int> lista;
    private IntStream intStream;

    public ListaLeniwa()
    {
        lista = new List<int>();
        intStream = new IntStream();
    }

    public int size()
    {
        return lista.Count;
    }

    virtual public int element(int i)
    {
        if(i<=size())
        {
            return lista[i-1];
        }
        else
        {
            while(i>size())
            {
                lista.Add(intStream.next());
            }
            return lista[i-1];
        }
    }

}
