#include<iostream>
#include<string>

using namespace std;

istream& clearline (istream &);

class ignore
{
private:
    int x;
    friend istream& operator >> (istream &, const ignore&);
public:
    ignore(int);
};

istream& operator>> (istream&, const ignore&);

ostream& coma (ostream &);

ostream& colon (ostream &);

class index
{
private:
    int x;
    unsigned int w;
    friend ostream& operator<< (ostream&, const index&);
public:
    index(int x, unsigned int w);
};

ostream& operator<< (ostream&, const index&);