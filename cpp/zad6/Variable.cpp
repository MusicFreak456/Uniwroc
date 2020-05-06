#include"Expression.hpp"

Variable::Variable(std::string id): id(id) {}

std::string Variable::to_string()
{
    return this->id;
}

std::vector<std::pair<std::string,double>> Variable::list;

double Variable::eval()
{
    for(std::pair<std::string,double> p: this->list)
    {
        if(p.first == this->id)
        {
            return p.second;
        }
    }
    throw std::invalid_argument("Value of " + this->id + " not found.");
}

void Variable::add_variable(std::string id,double value)
{
    list.push_back(std::pair<std::string,double>(id,value));
}
void Variable::remove_variable(std::string id)
{
    for(auto index = list.begin(); index != list.end();)
    {
        if(index->first == id)
        {
            index = list.erase(index);
        }
        else
        {
            ++index;
        }
        
    }
}
void Variable::change_variable_value(std::string id,double value)
{
    remove_variable(id);
    add_variable(id,value);
}