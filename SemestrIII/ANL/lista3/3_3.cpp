#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double old_way(double q, double r) {
    return pow(r - sqrt(pow(q, 3) + pow(r, 2)), 1.0/3.0);
}

int main() {
    double q = pow(10, -5);
    double r = 100.0;

    cout.precision(40);
    cout << fixed << "Dla starego wzoru:" << endl;
    cout << old_way(q, r) << endl;
    return 0;
}