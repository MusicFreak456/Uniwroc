using System;
using Pair;


public class Slownik<K,V> : IList<Pair<K,V>> where K: IComparable<K>
{
    private Slownik<K,V> next;
    private Pair<K,V> pair;

    public Slownik()
    {
        this.next=null;
        this.pair=null;
    }
    private Slownik(Pair<K,V> pair)
    {
        this.next=null;
        this.pair=pair;
    }
    public void addBack(K key,V value)
    {
        if (this.pair!=null && this.pair.first.CompareTo(key)==0) return;
        if(this.next==null)
        {
            Pair<K,V> new_pair = new Pair<K,V>(key,value);
            this.next = new Slownik<K,V>(new_pair);
        }
        else this.next.addBack(key,value);
    }
    public void addBack(Pair<K,V> pair)
    {
        if (this.pair!=null && this.pair.first.CompareTo(pair.first)==0) return;
        if(this.next==null)
        {
            this.next = new Slownik<K,V>(pair);
        }
        else this.next.addBack(pair);
    }
    public Pair<K,V> fetch(K key)
    {
        if(this.pair!=null && key.CompareTo(this.pair.first)==0)
        {
            return this.pair;
        }
        else if(this.next!=null)
        {
            return this.next.fetch(key);
        }
        else return null;
    }
    public void remove (K key)
    {
        if(this.next!=null && (this.next.pair.first.CompareTo(key)==0))
        {
            this.next=this.next.next;
        }
        else if(this.next!=null)
        {
            this.next.remove(key);
        }
        else return;
    }

    public bool isEmpty()
    {
        return this.next == null;
    }
    
    public override string ToString()
    {
        string output = "";
        if(this.pair!=null)
        {
            output+="(";
            output+=Convert.ToString(pair.first);
            output+=" . ";
            output+=Convert.ToString(pair.second);
            output+=") ";
        };
        if(this.next!=null) 
        {
            return output + this.next.ToString();
        }
        else return output;

    }

}
