import java.util.LinkedList;


public class Buffer<T>
{
    private LinkedList<T> buffer;
    private int capacity;

    public Buffer(int capacity)
    {
        this.buffer = new LinkedList<T>();
        this.capacity = capacity;
    }

    public boolean isFull()
    {
        return buffer.size() == this.capacity;
    }

    public boolean isEmpty()
    {
        return buffer.isEmpty();
    }

    public synchronized void put (T newValue) throws InterruptedException
    {
        while(this.isFull())
        {
            this.wait();
        }

        
        buffer.addLast(newValue);
        System.out.println("#Producer put: " + newValue);
        this.notify();
    }

    public synchronized T take() throws InterruptedException
    {
        while(this.isEmpty())
        {
            this.wait();
        }


        T returnValue = buffer.pollFirst();
        System.out.println("#Consumer took: " + returnValue);
        this.notify();
        return returnValue;
    }

}