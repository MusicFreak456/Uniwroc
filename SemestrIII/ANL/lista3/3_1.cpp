#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double old_way(double x) {
    return 4 * pow(cos(x),2) - 3;
}

double new_way(double x) {
    return cos(3 * x) / cos(x);
}

int main() {
    double prev_x_old = 2;
    double prev_x_new = 2;

    cout.precision(20);
    cout << fixed << "Dla starego wzoru:" << endl;
    for(int i = 1; i <= 30; i++) {
        cout << old_way(prev_x_old) << endl;
    }

    cout << endl << endl << "Dla nowego wzoru:" << endl;
    for(int i = 1; i <= 30; i++) {
        cout << new_way(prev_x_new) << endl;
    }

    return 0;
}