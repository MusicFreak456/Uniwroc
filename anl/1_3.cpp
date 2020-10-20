#include<iostream>
#include<cstdlib>
#include<cmath>

double next_value(double prev_value, double prev_prev_value) {
    return ( 1.0/7 ) * ( 69 * prev_value + 10 * prev_prev_value);
}

int main() {
    double result;
    double y_prev = -(1.0 / 7);
    double y_prev_prev = 1.0;

    std::cout << "y0 = " << y_prev << std::endl;
    std::cout << "y1 = " << y_prev_prev << std::endl;

    for(int i = 2; i <= 50; i++) {
        double new_value = next_value(y_prev, y_prev_prev);
        std::cout << "y" << i << " = " << new_value << std::endl;
        y_prev_prev = y_prev;
        y_prev = new_value;
    }


    return 0;
}