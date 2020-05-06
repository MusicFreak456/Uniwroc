import java.util.Random;

public class Consumer extends Thread
{
    private Buffer<String> buffer;

    public Consumer(Buffer<String> buffer)
    {
        this.buffer = buffer;
    }

    public void run()
    {
        for(int i = 1 ; i <=10 ;i ++)
        {
            try
            {
                String phrase;
                phrase = buffer.take();
                
            }
            catch(InterruptedException e)
            {
                e.printStackTrace();
            }
        }
    }


}