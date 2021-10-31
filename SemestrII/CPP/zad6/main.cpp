#include<iostream>
#include<cstdlib>
#include"Expression.hpp"

using namespace std;

int main()
{
    cout << "Test pojedynczej liczby: " << endl;
    Expression * w = new Number(4);
    cout << w->eval() << endl;
    cout << w->to_string() << endl;

    cout << "Test zmiennej: " << endl;
    Expression * f = new Variable("x");
    cout << f->to_string() << " = ";
    Variable::add_variable("x", 2);
    cout << f->eval() << endl;
    Variable::change_variable_value("x",4);
    cout << f->eval() << endl;
    Variable::remove_variable("x");
    try
    {
        cout << f->eval();
    }
    catch(const std::exception& e)
    {
        std::cerr << e.what() << '\n';
    }

    cout << "Test stałych" << endl;
    Expression * con = new Pi();
    cout << con->eval() << " = ";
    cout << con->to_string() << endl;
    con = new e();
    cout << con->eval() << " = ";
    cout << con->to_string() << endl;
    con = new Phi();
    cout << con->eval() << " = ";
    cout << con->to_string() << endl;
    
    cout << "Test operatorów unarnych: " << endl;
    Expression * k = new Cos(new Pi());
    cout << k->to_string() <<" = ";
    cout << k->eval() << endl;
    k = new Inverse(new Phi());
    cout << k->to_string() <<" = ";
    cout << k->eval() << endl;
    k = new Abs(new Number(-42));
    cout << k->to_string() <<" = ";
    cout << k->eval() << endl;
    cout << "Testy operatorów binarnych: " << endl;
    Expression * l = new Expt(new Number(2), new Number(3));
    cout << l->to_string() << " = ";
    cout << l->eval() << endl;
    l = new Mult(new Number(6), new Number(7));
    cout << l->to_string() << " = ";
    cout << l->eval() << endl;

    cout << "Testy z zadania: " << endl;
    Expression * wyr[4];
    wyr[0] = new Div(new Mult(new Sub(new Variable("x"), new Number(1)), new Variable("x")),new Number(2));
    cout << wyr[0]->to_string() << endl;
    wyr[1] = new Div(new Add(new Number(3),new Number(5)), new Add(new Number(2), new Mult(new Variable("x"),new Number(7))));
    cout << wyr[1]->to_string() << endl;
    wyr[2] = new Add(new Number(2), new Sub(new Mult(new Variable("x"),new Number(7)),new Add(new Mult(new Variable("y"),new Number(3)),new Number(5))));
    cout << wyr[2]->to_string() <<endl;
    wyr[3] = new Div(new Cos(new Mult(new Add(new Variable("x"),new Number(1)),new Variable("x"))),new Exp(new Expt(new Variable("x"),new Number(2))));
    cout << wyr[3]->to_string() << endl;

    Variable::add_variable("x",0);

    //W zadaniu nie było jasno wyspecyfikowane co zrobić ze zmienną "y", więc
    //po prostu zostawiłem ją bez wartości
    for(double x=0; x<1.01; x+=0.01)
    {
        cout << endl;
        cout << " x = " << x << endl;
        Variable::change_variable_value("x",x);
        for(int i = 0 ; i <=3 ; i++)
        {
            try
            {
                cout << wyr[i]->eval() << endl;
            }
            catch(const std::exception& e)
            {
                std::cerr << e.what() << '\n';
            }
            
        }
    }
    return 0;
}