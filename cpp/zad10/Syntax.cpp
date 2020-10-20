#include"Syntax.hpp"

namespace rpn_calculator
{

    istream& operator>> (istream& is, Syntax& stx)
    {
        getline(is, stx.expression);
        return is;
    }

    ostream& operator<< (ostream& os, Syntax& stx)
    {
        string to_display;
        stx >> to_display;
        os << to_display;
        return os;
    }

    Syntax Syntax::operator>>(string& out)
    {
        string to_return;
        uint i;
        uint j;

        for(i = 0; i < this->expression.length(); i++)
        {
            if(this->expression[i] != ' ') break;
        }

        if(i != 0) this->expression = this->expression.substr(i);

        for(j = 0; j < this->expression.length() ;j++)
        {
            char curr = this->expression[j];
            if(curr == ' ') break;
            else
            {
                to_return.push_back(curr);
            }
            
        }

        if(j != 0)
        {
            this->expression = this->expression.substr(j);
        }

        out = to_return;
 
        return *this;
    }
}