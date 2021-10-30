using System;

class RandomStream :IntStream
{
    private Random random;

    public RandomStream()
    {
        this.random = new Random();
        this.value=random.Next();
    }

    public override int next()
    {
        int old_value=this.value;
        this.value=random.Next();

        return old_value;
    }

    public override void reset()
    {

    }
}