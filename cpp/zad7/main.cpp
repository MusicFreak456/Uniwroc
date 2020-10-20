#include<cstdlib>
#include<iostream>

#include "Rational.hpp"

using namespace calculations;

using namespace std;

int main()
{
    Rational a(2,4);

    cout << "a=2/4:" << endl;
    a.display();
    cout << "Cast to double:" <<endl;
    double ad = a;
    cout << ad << endl;
    cout << "Cast to int:" <<endl;
    int ai = (int)a;
    cout << ai <<endl;

    cout << "b=-a" <<endl;
    Rational b = -a;
    b.display();

    cout << "-2/-4:"<<endl;
    Rational c(-2,-4);
    c.display();

    cout << "Integer 3:" <<endl;
    Rational d(3);
    d.display();

    cout << "e=!a" <<endl;
    Rational e = !a;
    e.display();

    cout << "f=a+d+b" <<endl;
    Rational f = a + d + b;
    f.display();

    cout << "g = d-a" << endl;
    Rational g = d - a;
    g.display();

    cout << "h = a*a" << endl;
    Rational h = a * a;
    h.display();

    cout << "i = d/2" << endl;
    Rational i = d / Rational(2);
    i.display();
    cout<< "Operator <<" << endl;
    cout << i << endl;

    cout << "k = 1/7" <<endl;
    Rational k(1,7);
    k.display();
    cout << k << endl;

    cout << "Exceptions tests:" <<endl;
    try
    {
        Rational j = Rational(INT32_MAX) + Rational(1);
        j.display();
    }
    catch(const RationalException& e)
    {
        std::cerr << e.what() << '\n';
    }
    try
    {
        Rational j(1,0);
        j.display();
    }
    catch(const RationalException& e)
    {
        std::cerr << e.what() << '\n';
    }

    return 0;
} 