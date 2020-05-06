#include "Expression.hpp"

Number::Number(double value): value(value)
{}

double Number::eval()
{
    return this->value;
}

std::string Number::to_string()
{
    return std::to_string(this->value);
}