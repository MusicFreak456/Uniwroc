#include<iostream>
#include<cstdlib>
#include<cmath>

double function(double x) {
    return 4040.0 * ( ( sqrt( pow(x, 11.0) + 1 ) - 1 ) / pow(x, 11.0) );
}

int main() {
    double result;
    double x = 0.001;

    std::cout << function(x) << std::endl;

    return 0;
}