#include<iostream>
#include<cstdlib>
#include<cmath>

float single_precision(float x) {
    float result = 12120.0 * ((x - (float)sin(x)) / (float)pow(x, 3));
    return result;
}

double double_precision(double x) {
    double result = 12120.0 * ((x - sin(x)) / pow(x, 3));
    return result;
}

int main() {

    for(int i= 11; i <= 20; i++){
        std::cout << single_precision(pow(10, -i)) << std::endl;
    }

    std::cout << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << std::endl;

    for(int i= 11; i <= 20; i++){
        std::cout << double_precision(pow(10, -i)) << std::endl;
    }

    return 0;
}