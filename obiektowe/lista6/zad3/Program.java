
public class Program 
{
    public static void main(String[] args) 
    {
        Buffer<String> newBuffer = new Buffer<String>(3);
        Producer newProducer = new Producer(newBuffer);   
        Consumer newConsumer = new Consumer(newBuffer);
        newProducer.start();
        newConsumer.start();
    }
}