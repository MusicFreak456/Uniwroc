#include<iostream>
#include<cstdlib>
#include<cmath>
#include <iomanip>

using namespace std;

double newton_step(double xn, double a) {
    return 0.5 * (3 * xn - a * pow(xn, 3));
}


int main() {

    double x[8] = {0.5, 0.5, 0.316, 1.2, 1.2, 0.9, 0.9, 0.7};
    double a[8] = {2,   3,   9,     2,   3,   3,   4,   4};

    for(int i=0; i<8; i++) {
        cout << endl << "x0: " << x[i] << endl; 
        cout << fixed << setprecision(20);

        for(int j = 1; j <= 10; j++){
            x[i] = newton_step(x[i], a[i]);
            cout << "i:" << j << " " << x[i] << endl;
        }

        cout << "expected: " << 1 / sqrt(a[i]) << endl;
    }

    return 0;
}