#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double next_integral(double prev_integral, int index) {
    return (1.0 / index) - 2020.0 * prev_integral;
}

int main() {
    double prev_integral = log(2021.0 / 2020.0);

    cout << "I0 = " << prev_integral << endl;

    for(int i = 1; i <= 20; i++ ) {
        prev_integral = next_integral(prev_integral, i);
        cout << "I" << i << " = " << prev_integral << endl;
    }

    return 0;
}