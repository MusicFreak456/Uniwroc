using System.Collections;

class ListaEnum<T> : IEnumerator
{
    ListElem<T> startelement;
    ListElem<T> current;
    public ListaEnum(ListElem<T> startelement)
    {
        this.startelement=startelement;
        this.current=startelement;
    }
    public bool MoveNext()
    {
        if(this.current.next!=this.startelement)
        {
            this.current=this.current.next;
            return true;
        }else return false;
    }
    public object Current
    {
        get
        {
            return this.current.value;
        }
    }
    public void Reset()
    {
        this.current = this.startelement;
    }
}