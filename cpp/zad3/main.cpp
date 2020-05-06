#include<iostream>
#include<cstdlib>
#include<string>
#include<vector>

using namespace std;

const vector<pair<int,string>> rzym = {
    {1000,"M"},{900,"CM"},{500,"D"},
    {400,"CD"},{100,"C"},{90,"XC"},
    {50,"L"},{40,"XL"},{10,"X"},
    {9,"IX"},{5,"V"},{4,"IV"},
    {1,"I"},
};

vector<int> convert(int argc,char **argv)
{
    vector<int> int_argv;
    for(int i=1; i<argc; i++)
    {
        try
        {
            string temp(argv[i]);
            int converted = stoi(temp);
            if(converted < 1 || converted > 3999)throw invalid_argument("Out of range");
            int_argv.push_back(converted);
        }
        catch(const std::out_of_range& e)
        {
            std::clog << argv[i] <<": Out of range" << '\n';
        }
        catch(const std::invalid_argument& e)
        {
            std::clog << argv[i] << ": " << e.what() << '\n';
        }
        
    }
    return int_argv;
}

string bin2rom(int x)
{
    string result;
    for(const pair<int,string> &p: rzym)
    {
        while(x-p.first >= 0)
        {
            result+=p.second;
            x-=p.first;
        }
    }
    return result;
}

int main(int argc, char **argv)
{
    if(argc <= 1)
    {
        clog << "No arguments given..." << endl;
        clog << "Usage: " << argv[0] << " numbers..." << endl;
        return 0;
    }
    vector<int> int_argv = convert(argc,argv);

    for (int &x : int_argv)
    {
        cout << bin2rom(x) << endl;
    }
    
    return 0;
}