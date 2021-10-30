using System;

class RandomWordStream
{
    private PrimeStream prime;
    private RandomStream random;
    private string word;

    public RandomWordStream()
    {
        prime = new PrimeStream();
        random = new RandomStream();
        word="";
        prime.next();
        for(int i=1;i<=2;i++)
        {
            word+=Convert.ToChar(random.next()%58+65);
        }
    }

    public string next()
    {
        string old_string=word;
        int next_prime=prime.next();

        for(int i=word.Length;i<next_prime;i++)
        {
            this.word+=Convert.ToChar(random.next()%58+65);
        }
        return old_string;
    }

}