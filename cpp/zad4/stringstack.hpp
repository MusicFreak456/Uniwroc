#include<cstdlib>
#include<iostream>

using namespace std;

class Stringstack
{
private:
    string * Table;
    int capacity;
    int size;
public:
    Stringstack();
    Stringstack(int);
    Stringstack(initializer_list<string>);
    Stringstack(const Stringstack&);
    Stringstack(Stringstack&&);

    Stringstack& operator=(const Stringstack&);
    Stringstack& operator=(Stringstack&&);

    void insert(string);

    string pop_head();
    string head();

    int get_size();
    int get_capacity();

    bool is_empty();

    Stringstack reverse();

    ~Stringstack();
};