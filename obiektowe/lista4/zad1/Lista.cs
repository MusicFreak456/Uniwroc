using System;
using System.Collections;


public class Lista<T> :IList<T>,IEnumerable
{
    private ListElem<T> element;


    public Lista()
    {
        this.element = new ListElem<T>();
    }

    public void addBack(T newVal)
    {
        ListElem<T> newelem = new ListElem<T>();
        newelem.value = newVal;
        newelem.prev=this.element.prev;
        newelem.next=this.element;
        this.element.prev.next = newelem;
        this.element.prev =newelem;
    }

    public void addFront(T newVal)
    {
        ListElem<T> newelem = new ListElem<T>();
        newelem.value = newVal;
        newelem.next = this.element.next;
        newelem.prev = this.element;
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

    public bool isEmpty()
    {
        return this.element.next == this.element;
    }

    public override string ToString()
    {
        string output = "[ ";
        ListElem<T> temp = this.element.next;

        while(temp!=this.element)
        {
            output+=Convert.ToString(temp.value);
            output+= " ";
            temp = temp.next;
        }
        output+= "]";

        return output;
    }

    public int Length
    {
        get
        {
            int output = new int();
            ListElem<T> temp = this.element.next;

            while(temp!=this.element)
            {
                output++;
                temp = temp.next;
            }
            return output;
        }
    }

    public T this[int index]
    {
        get
        {
            ListElem<T> temp = this.element.next;
            while(index!=0)
            {
                temp = temp.next;
                index--;
            }
            return temp.value;
        }
    }

    public IEnumerator GetEnumerator()
    {
        return new ListaEnum<T>(element);
    }

}
