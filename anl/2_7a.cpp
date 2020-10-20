#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double old_way(double x) {
    return pow(x, 3) - sqrt(pow(x,6) + 2020.0);
}

double new_way(double x) {
    return -2020 / (pow(x, 3) + sqrt(pow(x,6) + 2020));
}

int main() {
    cout.precision(30);
    cout << fixed << "Dla starego wzoru:" << endl;
    for(int i = 1; i <= 15; i++) {
        cout << old_way(pow(10,i)) << endl;
    }

    cout << endl << endl << "Dla nowego wzoru:" << endl;
    for(int i = 1; i <= 15; i++) {
        cout << new_way(pow(10,i)) << endl;
    }

    return 0;
}