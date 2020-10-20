#ifndef OPERAND_HPP
#define OPERAND_HPP

#include<string>
#include<cmath>
#include<map>
#include"Expression.hpp"

using std::string;
using std::stack;
using std::map;

namespace rpn_calculator
{
    class Operand :public Expression
    {
    public:
        virtual void evaluate(stack<double> &)=0;
    };

    class Number :public Operand
    {
    private:
        double value;
    public:
        Number(string);
        void evaluate(stack<double> &);
    };

    class Variable :public Operand
    {
    private:
        static map<string,double> var_set;
        string label;
    public:
        Variable(string);
        static void add_var(string, double);
        static void clear();
        static bool member(string);

        void evaluate(stack<double> &);
    };

    class Const :public Operand
    {
    private:
        const double PI = M_PI;
        const double E = M_E;
        const double PHI = 1.618033988750;
        string label;
    public:
        Const(string);
        void evaluate(stack<double> &);
    };
}

#endif