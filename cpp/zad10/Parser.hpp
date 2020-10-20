#ifndef PARSER_HPP
#define PARSER_HPP

#include<set>
#include<queue>
#include<iostream>
#include<stdexcept>

#include"Syntax.hpp"
#include"Expression.hpp"
#include"Operand.hpp"
#include"Function.hpp"

using std::queue;
using std::set;

namespace rpn_calculator
{
    class Parser
    {
    private:
        const set<string> ops_keywords = {"+","-","*","/",
        "modulo","min","max","log","pow","abs","sgn","floor","ceil","sin","cos",
        "atan","acot","ln","exp"};
        const set<string> const_keywords = {"pi","e","phi"};
        const set<string> other_keywords = {"assign","print","clear", "exit"};
    public:
        queue<Expression*> parse(Syntax&);
        void name_check(string);
    };
}

#endif