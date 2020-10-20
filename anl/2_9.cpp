#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double old_way(double x, int k) {
    return pow(2.0, k) * sqrt(2.0 * (1.0 - sqrt( 1.0 - pow( x/pow(2.0,k), 2))));
}

double new_way(double x, int k) {
    double a = pow((x / pow(2.0,k)), 2);
    return pow(2.0, k) * sqrt( 2.0 * ( a / (1 + sqrt(1 - a))));
}

int main() {
    double prev_x_old = 2;
    double prev_x_new = 2;

    cout.precision(20);
    cout << fixed << "Dla starego wzoru:" << endl;
    for(int i = 1; i <= 30; i++) {
        prev_x_old = old_way(prev_x_old, i);
        cout << prev_x_old << endl;
    }

    cout << "PI: " << M_PI << endl;

    cout << endl << endl << "Dla nowego wzoru:" << endl;
    for(int i = 1; i <= 30; i++) {
        prev_x_new = new_way(prev_x_new, i);
        cout << prev_x_new << endl;
    }
    cout << "PI: " << M_PI << endl;

    return 0;
}