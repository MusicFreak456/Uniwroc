import javax.lang.model.util.ElementScanner6;

public class AscList<T extends Comparable<T>>
{
    private AscList<T> next;
    private T value;

    public AscList()
    {
        this.next = null;
        this.value = null;
    }

    public AscList(T newValue)
    {
        this.next = null;
        this.value = newValue;
    }

    public void add(T newValue)
    {
        if(this.next == null)
        {
            this.next = new AscList<T>(newValue);
            return;
        }
        int comparison = newValue.compareTo(this.next.value);

        if (comparison == 0)
        {
            throw new IllegalArgumentException("Równy obiekt istnieje na liście");
        }
        else if (comparison < 0)
        {
            AscList<T> newElem = new AscList<T>(newValue);
            newElem.next = this.next; 
            this.next = newElem;
            return;
        }
        else
        {
            this.next.add(newValue);
        }
    }

    public T getElem()
    {
        if(this.next == null) return null;
        T returnValue = this.next.value;
        this.next = this.next.next;
        return returnValue;
    }

}

