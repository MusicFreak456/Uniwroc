#include<iostream>
#include<algorithm>
#include<fstream>
#include<string>
#include<vector>
#include"manipulators.hpp"

using namespace std;

int main()
{
    ifstream in_file;
    vector<pair<int,string>> vector_of_lines;
    int line_nr = 1;
    string line;

    in_file.open("test.txt");
    getline(in_file,line);

    while (!in_file.eof())
    {
        vector_of_lines.push_back(pair<int,string>(line_nr,line));
        getline(in_file,line);
        line_nr++;
    }

    in_file.close();
    
    sort(
        vector_of_lines.begin(),
        vector_of_lines.end(), 
        [](const pair<int,string> &left, const pair<int,string> &right){
            return left.second < right.second;
        });

    ofstream out_file;
    out_file.open("result.txt");

    for(const pair<int,string> &x : vector_of_lines)
    {
        out_file << index(x.first, 1) << colon << x.second << endl;
    }

    out_file.close();
        
}