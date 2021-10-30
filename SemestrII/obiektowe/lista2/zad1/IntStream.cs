using System;

class IntStream
{
    protected int value;

    public IntStream()
    {
        value=0;
    }

    protected bool isPrime(int number)
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

    virtual public int next()
    {
        if(!this.eos())
        {
            value++;
            return value-1;
        }
        else
        {
            return value;
        }
    }

    virtual public bool eos()
    {
        return value + 1L > int.MaxValue;
    }

    virtual public void reset()
    {
        value=0;
    }

}