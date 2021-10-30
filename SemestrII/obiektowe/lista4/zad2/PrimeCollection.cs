using System;
using System.Collections;

class PrimeCollection : IEnumerable
{
    private int startvalue;

    public PrimeCollection()
    {
        this.startvalue = 1;
    }
    public IEnumerator GetEnumerator()
    {
        return new PrimeCollectionEnum(startvalue);
    }

}