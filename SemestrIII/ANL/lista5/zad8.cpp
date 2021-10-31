#include<iostream>
#include<cstdlib>
#include<cmath>
#include<iomanip>

using namespace std;

double f1(double x) {
    return pow(x, 3) - 1;
}

double f1_prim(double x) {
    return 3 * pow(x, 2);
}

double newton_step(double xn, double fx, double fpx) {
    return xn - fx / fpx;
}

double rate_of_convergance(double curr, double prev, double prev_prev, double alpha) {
    return log(abs(curr - alpha) / (prev - alpha)) / log(abs(prev - alpha) / (prev_prev - alpha));
}

int main() {
    double xn = 0.5;

    cout << fixed << setprecision(30);

    for(int i=0; i <=10; i++ ){
        xn = newton_step(xn, f1(xn), f1_prim(xn));
        if(f1(xn) < pow(10 , -20)){
            break;
        }
        cout << xn << endl;
    }

    cout << endl << "Rząd zbieżności: " << rate_of_convergance( 1.000000000006031175558973700390, 1.000002455837752046718946985493, 1.001568749570643568347350083059, 1.0);

    return 0;
}