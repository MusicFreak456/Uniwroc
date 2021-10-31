#include<iostream>
#include<ctime>
#include"binary.hpp"

using namespace binf;
using std::cout;
using std::endl;

int main()
{
    srand(time(NULL));
    OutputStream ostr("temp");

    for(int i = 1; i<=10 ;i++)
    {
        int a = (int)rand();
        cout << a << endl;
        ostr << a; 
    }

    ostr.close();
    cout << "----------------------------------" <<endl;

    InputStream istr1("temp");

    for (int i = 1; i <= 10; i++)
    {
        int a;
        istr1 >> a;
        cout << a << endl;
    }
    istr1.close();

    cout << "----------------------------------" <<endl;

    InputStream istr2("temp");
    uint8_t byte;
    istr2 >> byte;
    while(!istr2.eof())
    {
        if(byte >= 33 && byte <= 126)
        cout << (char)byte<< " ";
        cout << std::hex << (int)byte<< " ";
        cout << std::dec << (int)byte;
        cout << endl;
        istr2 >> byte;
    }
    
    return 0;
}