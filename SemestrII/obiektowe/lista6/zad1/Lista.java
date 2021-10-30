import java.io.*;

public class Lista<T extends Serializable> implements Serializable
{
    private Element<T> element;
    private static final long serialVersionUID = 1L;

    public Lista()
    {
        this.element = new Element<T>();
    }

    public void addBack(T newVal)
    {
        Element<T> newelem = new Element<T>();
        newelem.value = newVal;
        newelem.prev=this.element.prev;
        newelem.next=this.element;
        this.element.prev.next = newelem;
        this.element.prev =newelem;
    }

    public void addFront(T newVal)
    {
        Element<T> newelem = new Element<T>();
        newelem.value = newVal;
        newelem.next = this.element.next;
        newelem.prev = this.element;
        this.element.next.prev = newelem;
        this.element.next = newelem;
    }
    public T popFront()
    {
        T returnValue = this.element.next.value;
        this.element.next = this.element.next.next;
        this.element.next.prev = this.element;
        return returnValue;
    }
    public T popBack()
    {
        T returnValue = this.element.prev.value;
        this.element.prev.prev.next = this.element;
        this.element.prev = this.element.prev.prev;
        return returnValue;
    }

    public boolean isEmpty()
    {
        return this.element.next == this.element;
    }

    public void printList()
    {
        Element<T> tmp= this.element.next;
        while(tmp!=this.element)
        {
            System.out.println(tmp.value);
            tmp = tmp.next;
        }
    }
}