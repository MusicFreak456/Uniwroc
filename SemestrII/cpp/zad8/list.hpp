#ifndef LIST_HPP
#define LIST_HPP
#include<iostream>


using namespace std;

template<class T>
class Asc
{
public:
    static bool compare(T a, T b){return a <= b;}
};

template<class T>
class Asc<T*>
{
public:
    static bool compare(T* a, T* b){return *a <= *b;}
};

template<>
class Asc<const char*>
{
public:
    static bool compare(const char* a, const char* b){return *(&a + 1) - a <= *(&b + 1) - b;}
};

template<class T>
class Desc
{
public:
    static bool compare(T a, T b){return a >= b;}
};

template<class T>
class Desc<T*>
{
public:
    static bool compare(T* a, T* b){return *a >= *b;}
};

template<>
class Desc<const char*>
{
public:
    static bool compare(const char* a, const char* b){return *(&a + 1) - a >= *(&b + 1) - b;}
};



template <class T>
class LinkedList
{
private:
    class Node
    {
    public:
        T value;
        Node *next;

        Node()
        {
            this->next = nullptr;
        }
    };

    Node start_node;
public:
    LinkedList(): start_node()
    {}

    LinkedList(initializer_list<T> list)
    {
        for(const T& x: list)
        {
            this->pushback(x);
        }
    }

    template<class K> friend ostream& operator<< (ostream&out ,LinkedList<K>& list);

    int length()
    {
        int count = 0;
        Node * curr_node = &(this->start_node);
        while (curr_node->next != nullptr)
        {
            count++;
            curr_node = curr_node->next;
        }

        return count;
    }

    void pushback(T value)
    {
        Node * new_node = new Node();
        new_node->value = value;
        Node * curr_node = &(this->start_node);
        while (curr_node->next != nullptr)
        {
            curr_node = curr_node->next;
        }

        curr_node->next = new_node; 
    }

    void insert(T val, int index)
    {
        Node * new_node = new Node();
        new_node->value = val;
        Node * curr_node = &(this->start_node);
        for(int i=0; i<index; i++)
        {
            curr_node = curr_node->next;
        }

        new_node->next = curr_node->next;
        curr_node->next = new_node;
    }

    void remove(T val)
    {
        Node * curr_node = &(this->start_node);
        while (curr_node->next != nullptr)
        {
            if(curr_node->next->value == val)
            {
                Node * temp = curr_node->next;
                curr_node->next = curr_node->next->next;
                delete(temp);  
                return;
            }
            curr_node = curr_node->next;
        }
        
    }

    int locate(T val)
    {
        int index = 0;
        Node * curr_node = &(this->start_node);
        while (curr_node->next != nullptr)
        {
            if(curr_node->next->value == val)
            {
                return index;
            }
            curr_node = curr_node->next;
            index++;
        }

        return -1;
    }

    template<class Z, typename C = Asc<T>>
    bool check()
    {
        Node * curr_node = &(this->start_node);
        while (curr_node->next != nullptr && curr_node->next->next != nullptr)
        {
            if(!C::compare(curr_node->next->value,curr_node->next->next->value))
            {
                return false;
            }
            curr_node = curr_node->next;
        }
        return true;
    }

    template<class Z, typename C = Asc<T>>
    void sort()
    {
        int length = this->length();
        Node * ielem = this->start_node.next;
        Node * jelem = this->start_node.next;

        for(int i = 0; i <= length-1 ; i++)
        {
            jelem = ielem;
            for(int j = 0; j <= length - 1 - i; j++)
            {
                if(C::compare(jelem->value,ielem->value))
                {
                    Z temp = jelem->value;
                    jelem->value = ielem->value;
                    ielem->value = temp;
                }
                jelem = jelem->next;
            }
            ielem = ielem->next;
        }
    }


};

template<class K>
ostream& operator<< (ostream&out ,LinkedList<K>& list)
{
    LinkedList<K> copy = list;
    while (copy.start_node.next != nullptr)
    {
        out << copy.start_node.next->value << " ";
        copy.start_node = *(copy.start_node.next);
    }
    return out;
}


#endif
