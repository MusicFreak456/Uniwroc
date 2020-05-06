import java.util.Random;

public class Producer extends Thread
{
    private Random random;
    private String phrase;
    private Buffer<String> buffer;

    public Producer(Buffer<String> buffer)
    {
        this.random = new Random();
        this.phrase = "";
        this.buffer = buffer;
    }

    public void run()
    {
        for(int i = 1 ; i <=10 ;i ++)
        {
            char newChar = (char)(Math.abs(random.nextInt()%26)+65);
            this.phrase+=Character.toString(newChar);
            try
            {
                
                this.buffer.put(phrase);
            }
            catch(InterruptedException e)
            {
                e.printStackTrace();
            }
        }
    }



}