#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double f(double x) {
    return pow(x, 2) - 2.0 * cos( 3.0 * x + 1.0);
}

double en(int n, double a0, double b0) {
    return pow(2.0, -n-1.0) * (b0 - a0);
}

void bisection(double a0, double b0){
    double a = a0;
    double b = b0;
    double e = pow(10, -5);
    bool inc = f(b0) >= 0;
    int i = 0;
    while(true) {
        double m = (a + b) / 2.0;

        if( inc ? f(m) > 0 : f(m) < 0){
            b = m;
        }
        else
        {
            a = m;
        }

        if(en(i, a0, b0) <= e){
            cout << m << endl;
            break;
        }
        i++;
    }
}

int main() {

    bisection(0, 0.5);
    bisection(-1, -0.5);

    return 0;
}