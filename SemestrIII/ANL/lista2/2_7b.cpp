#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

int factorial(int n){
    int result = 1;

    for(int i = 2; i <= n; i++ ) {
        result *= i;
    }

    return result;
}

double old_way(double x) {
    return pow(x, -4) * (cos(x) - 1 + ( pow(x, 2) / 2));
}

double new_way(double x) {
    double result = 0;
    for(int i = 0; i <= 10; i++) {
        result += pow(-1.0, i) * pow(x, i) / factorial(4 + 2 * i);
    }

    return result;
}

int main() {
    cout.precision(30);
    cout << fixed << "Dla starego wzoru:" << endl;
    for(int i = 1; i <= 15; i++) {
        cout << old_way(pow(10,-i)) << endl;
    }

    cout << endl << endl << "Dla nowego wzoru:" << endl;
    for(int i = 1; i <= 15; i++) {
        cout << new_way(pow(10,-i)) << endl;
    }

    return 0;
}