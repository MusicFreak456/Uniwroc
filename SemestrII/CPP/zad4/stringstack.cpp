#include"stringstack.hpp"

Stringstack::Stringstack(int capacity): capacity(capacity), size(0)
{
    if(this->capacity > 100 || this->capacity < 1) throw range_error("Capacity must be between 1 and 100");
    this->Table = new string[this->capacity];
}

Stringstack::Stringstack() : Stringstack(1){}

Stringstack::Stringstack(initializer_list<string> list) : Stringstack(list.size())
{
    for(string x: list)
    {
        this->Table[size] = x;
        this->size++;
    }
}

Stringstack::Stringstack(const Stringstack& stack): Table(new string[stack.capacity]), capacity(stack.capacity), size(stack.size)  
{
    for(int i=0 ;i <capacity;i++)
    {
        this->Table[i] = stack.Table[i];
    }
}

Stringstack::Stringstack(Stringstack&& stack) : Table(stack.Table), capacity(stack.capacity), size(stack.size)
{
    stack.Table = nullptr;
}

Stringstack& Stringstack::operator=(const Stringstack& stack)
{
    this->capacity = stack.capacity;
    this->size = stack.size;
    this->Table = new string[this->capacity];
    for(int i = 0 ; i < this->capacity; i++)
    {
        this->Table[i] = stack.Table[i];
    }
    return *this;
}
Stringstack& Stringstack::operator=(Stringstack&& stack)
{
    this->capacity = move(stack.capacity);
    this->size = move(stack.size);
    this->Table = move(stack.Table);
    stack.Table = nullptr;
    return *this;
}

Stringstack::~Stringstack()
{
    delete[] Table;
}

bool Stringstack::is_empty()
{
    return this->size == 0;
}

void Stringstack::insert(string new_value)
{
    if(size<capacity)
    this->Table[this->size++] = new_value;
    else throw range_error("Stack is full");
}


string Stringstack::pop_head()
{
    if(!this->is_empty())
    {
        this->size--;
        return this->Table[this->size];
    }
    else throw range_error("Stack is empty");
}
string Stringstack::head()
{
    if(!this->is_empty())
    return this->Table[this->size-1];
    else throw range_error("Stack is empty");
}
int Stringstack::get_size()
{
    return this->size;
}
int Stringstack::get_capacity()
{
    return this->capacity;
}

Stringstack Stringstack::reverse()
{
    Stringstack temp(*this);
    Stringstack new_stack(this->capacity);
    while (!temp.is_empty())
    {
        new_stack.insert(temp.pop_head());
    }
    return new_stack;
}