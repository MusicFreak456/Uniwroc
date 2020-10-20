#include<iostream>
#include<queue>
#include"Syntax.hpp"
#include"Expression.hpp"
#include"Parser.hpp"
#include"Evaluator.hpp"

using std::string;
using std::cin;
using std::cout;
using std::endl;
using std::queue;
using namespace rpn_calculator;

int main()
{
    Syntax expr;
    Parser parser;
    Evaluator evaluator;

    while (true)
    {
        try
        {
            cout << "> ";
            cin >> expr;

            string command;
            expr >> command;

            if(command == "print")
            {
                cout << evaluator.eval(parser.parse(expr)) << endl;
            }
            else if (command == "assign")
            {
                double value = evaluator.eval(parser.parse(expr));
                string label;
                expr >> label;
                parser.name_check(label);
                Variable::add_var(label,value);
                std::clog << label << " <- " << value <<endl;

            }
            else if(command == "clear")
            {
                Variable::clear();
            }
            else if(command == "exit")
            {
                break;
            }
            else
            {
                std::clog << "Unknown command: " + command << endl;
            }
        }
        catch(const std::exception& e)
        {
            std::cerr << e.what() << '\n';
        }
    }

    return 0;
}