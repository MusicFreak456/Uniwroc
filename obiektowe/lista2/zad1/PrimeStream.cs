using System;

class PrimeStream : IntStream
{
    public PrimeStream()
    {
        this.value=1;
    }

    public override int next()
    {
        if(eos()) return this.value;
        else
        {
            for (int i = this.value+1; i < int.MaxValue; i++)
            {
                if(isPrime(i))
                {
                    this.value=i;
                    return i;
                }
            }
        }

        return 0;
    }

    public override bool eos()
    {
        return value == int.MaxValue;
    }

    public override void reset()
    {
        this.value=1;
    }
}