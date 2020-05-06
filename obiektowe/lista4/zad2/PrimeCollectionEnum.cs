using System;
using System.Collections;

class PrimeCollectionEnum :IEnumerator
{
    int startValue;
    int current;

    private bool isPrime(int number)
    {
        double root = System.Math.Sqrt(number);
        for (int i = 2; i <= root; i++)
        {  
            if(number%i==0)
            {
                return false;
            }
            
        }
        return true;
    }
    
    private int next(int start)
    {

        for (int i = start+1; i < int.MaxValue; i++)
        {
            if(isPrime(i))
            {
                return i;
            }
        }
        return default(int);
    }
    

    public PrimeCollectionEnum(int startvalue)
    {
        this.startValue = startvalue ;
        this.current = startValue;
    }
    public bool MoveNext()
    {
        if(this.current == Int32.MaxValue)
        {
            this.current=this.startValue;
            return false;
        }
        else
        {
           this.current = next(this.current);
           return true;
        }

    }
    public object Current
    {
        get
        {
            return this.current;
        }        
    }
    public void Reset()
    {
        this.current = startValue;
    }

}
//