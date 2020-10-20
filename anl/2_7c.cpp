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
    return (log(x) / log(5)) - 6;
}

double new_way(double x) {
    double result = 0;
    for(int i = 0; i <= 10; i++) {
        result += pow(-1.0, i) * pow(x, i) / i;
    }

    return result / log(5);
}

int main() {
    cout.precision(30);
    cout << fixed << "Dla starego wzoru:" << endl;
    for(int i = 1; i <= 20; i++) {
        cout << old_way( pow(5 + pow(10,-i), 6)) << endl;
    }

    cout << endl << endl << "Dla nowego wzoru:" << endl;
    for(int i = 1; i <= 20; i++) {
        cout << new_way( pow(5 + pow(10,-i), 6))  << endl;
    }

    return 0;
}