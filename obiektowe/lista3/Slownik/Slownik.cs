using System;
using Pair;

namespace Slownik
{
    public class Slownik<K,V> where K: IComparable<K>
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
        public void add(K key,V value)
        {
            if (this.pair!=null && this.pair.first.CompareTo(key)==0) return;
            if(this.next==null)
            {
                Pair<K,V> new_pair = new Pair<K,V>(key,value);
                this.next = new Slownik<K,V>(new_pair);
            }
            else this.next.add(key,value);
        }
        public void add(Pair<K,V> pair)
        {
            if (this.pair!=null && this.pair.first.CompareTo(pair.first)==0) return;
            if(this.next==null)
            {
                this.next = new Slownik<K,V>(pair);
            }
            else this.next.add(pair);
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
        
        public void display()
        {
            if(this.pair!=null)System.Console.WriteLine("{0} : {1}",this.pair.first,this.pair.second);
            if(this.next!=null) this.next.display();
        }

    }
}
