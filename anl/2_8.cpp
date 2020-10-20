#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double function(double x) {
    return 4040.0 / (sqrt(pow(x, 11) + 1) + 1);
}

int main() {
    double result;
    double x = 0;

    for(int i = 1; i <= 10; i++ ) {
        cout << function(pow(10, -i)) << endl;
    }

    return 0;
}