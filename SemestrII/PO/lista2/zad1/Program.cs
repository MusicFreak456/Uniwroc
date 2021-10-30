using System;

class Program
{
    static void Main(string[] args)
    {
        RandomWordStream stream = new RandomWordStream();
        System.Console.WriteLine(stream.next());
        System.Console.WriteLine(stream.next());
        System.Console.WriteLine(stream.next());
        System.Console.WriteLine(stream.next());
        System.Console.WriteLine(stream.next());
        
    }
}
