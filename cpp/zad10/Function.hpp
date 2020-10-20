#ifndef FUNCTION_HPP
#define FUNCTION_HPP

#include<stdexcept>
#include<string>
#include<stack>
#include<cmath>
#include"Expression.hpp"

using std::string;
using std::stack;

namespace rpn_calculator
{
    class Function :public Expression
    {
    private:
        string op_symbol;
    public:
        Function(string);
        void evaluate(stack<double>&);
    };
}

#endif