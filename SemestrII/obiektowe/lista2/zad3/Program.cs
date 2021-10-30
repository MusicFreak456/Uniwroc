using System;

class BigNum
{
    private string number;

    public BigNum(int n)
    {
        number = "";
        while(n!=0)
        {
            number=number.Insert(0,Convert.ToString(Convert.ToChar(n%10+Convert.ToInt32('0'))));
            n/=10;
        }
    }

    private BigNum(string number)
    {
        this.number=number;
    }

    public void Display()
    {
        System.Console.WriteLine(number);
    }

    public static BigNum operator +(BigNum a, BigNum b)
    {
        int aLength = a.number.Length;
        int bLength = b.number.Length;
        int newSize;
        string newNumber;

        if(aLength>bLength)
        {
            newSize=aLength;
            newNumber=a.number;

            

        }

        return a;
    }

}

class Program
{
    static void Main(string[] args)
    {
        BigNum firstNumber = new BigNum(10);
        BigNum secondNumber = new BigNum(20);

        secondNumber = firstNumber + secondNumber;

    }
}
