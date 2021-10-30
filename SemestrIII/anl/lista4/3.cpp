#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double f(double x) {
    return x - 0.49;
}

double en(int n, double a0, double b0) {
    return pow(2.0, -n-1.0) * (b0 - a0);
}


int main() {
    double a0 = 0;
    double b0 = 1;
    double a = a0;
    double b = b0;
    double x0 = 0.49;

    for(int i=0; i<5; i++) {
        double m = (a + b) / 2.0;
        
        if(f(m) > 0){
            b = m;
        }
        else
        {
            a = m;
        }

        cout << m << " error= " << abs(m - x0) << ", expected max error= " << en(i, a0, b0) <<  endl;
    }

    return 0;
}