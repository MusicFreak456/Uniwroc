using System;

class ListElem<T>
{
    public T value;
    public ListElem<T> next;
    public ListElem<T> prev;

    public ListElem()
    {
        next=this;
        prev=this;
    }
}