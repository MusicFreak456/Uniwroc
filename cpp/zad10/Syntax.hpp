#ifndef SYNTAX_HPP
#define SYNTAX_HPP

#include<string>
#include<iostream>

using std::string;
using std::istream;
using std::ostream;
namespace rpn_calculator
{
    class Syntax
    {
    private:
        string expression;
        friend istream& operator>> (istream& is, Syntax& stx);
        friend ostream& operator<< (ostream& os, Syntax& stx);
    public:
        Syntax operator>>(string&);
    };

}

#endif
