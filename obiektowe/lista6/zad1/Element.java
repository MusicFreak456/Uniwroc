import java.io.*;

public class Element<T extends Serializable> implements Serializable
{
    private static final long serialVersionUID = 2L;
    public T value;
    public Element<T> next;
    public Element<T> prev;

    public Element()
    {
        next=this;
        prev=this;
    }
}