#include<cstdlib>
#include<iostream>
#include<vector>


void test(std::vector<std::vector<int>> Tab){
    std::cout << Tab[1][1] << '\n';
    Tab[1][1] = 2;
    std::cout << Tab[1][1] << '\n';
}

int main(){
    std::vector<std::vector<int>> Tab;
    Tab.reserve(10);
    for (int i = 0; i < 5; i++)
    {
        std::vector<int> temp;
        temp.reserve(10);
        Tab.push_back(temp);
        for (int j = 0; j < 5; j++)
        {
            Tab[i].push_back(1);
        }
        
    }

    test(Tab);
    
}