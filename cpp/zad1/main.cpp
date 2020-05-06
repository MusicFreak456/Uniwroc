#include<cstdlib>
#include<cmath>
#include<iostream>
#include<vector>
#include<cstdint>

using namespace std;

bool is_prime(int64_t number)
{

    for (int64_t i = 2; i*i <= number; i++)
    {
        if(number%i==0)return false;
    }

    return true;
    
}

int64_t string_to_int(string input_string)
{
    int lenght=input_string.length();
    int sign = 1;

    if(input_string[0]=='-')
    {
        sign = -1;
        lenght--;
        input_string=input_string.substr(1,lenght);
    }

    int64_t result;
    int64_t power_of_ten = pow(10,lenght-1);
    
    result=0;
    for (int i = 0; i < lenght; i++)
    {
        int digit = input_string[i]-'0';

        if(digit<0 || digit>9) throw invalid_argument(" contains non-digit characters");
        if((sign==1 && digit*power_of_ten+result-INT64_MAX>0) || (sign==-1 && digit*power_of_ten+result-INT64_MIN>0)) throw invalid_argument(" is out of range");

        result+=digit*power_of_ten;
        power_of_ten/=10;
    }
    
    return result*sign;
}

vector<int64_t> prime_distr(int64_t number)
{
    vector<int64_t> result;
    if(number<0)
    {
        result.push_back(-1);
    }

    int64_t root = sqrt(abs((double)number))+1;

    for(int64_t i=2;i<=root;i++)
    {
        while (number%i==0)
        {
            result.push_back(i);
            number/=i;
        }
        if(number==1||number==-1)break;
    }
    if(result.size()==0||(number!=1&&number!=-1))
    {
        if(number>0)result.push_back(number);
        else result.push_back(-number);
    }

    return result;
}


int main(int argc, char **argv)
{
    if(argc<=1)
    {
        cerr << "No arguments given" << endl;
        cerr << "Usage: " << argv[0] << " number..." << endl;
        return 1;
    }
    
    vector<string> str_argv;
    for (int i = 1; i < argc; i++)
    {
        string new_string;
        new_string.assign(argv[i]);
        str_argv.push_back(new_string);
    }

    for(string&str_number: str_argv)
    {
        try
        {
            int iter=0;
            int64_t number=string_to_int(str_number);
            cout << number << " = ";
            vector<int64_t> distr = prime_distr(number);
            for(int64_t &factor: distr)
            {
                cout << factor;
                iter++;
                if(iter!=distr.size())cout << " * ";
            }
            cout << endl;

        }
        catch(const invalid_argument& e)
        {
            std::cerr << "\"" << str_number <<"\"" << e.what() << '\n';
        }
    }
    

    return 0;
}