#include<iostream>
#include<cstdlib>
#include<cmath>
#include <iomanip>

using namespace std;

pair<double, double> old_way(double a, double b, double c) {
    double x1, x2;
    double sqrt_delta = sqrt(b*b - 4*a*c);

    if(sqrt_delta >= 0 && a != 0)
    {
        x1 = (-b - sqrt_delta) / (2 * a);
        x2 = (-b + sqrt_delta) / (2 * a);

        return make_pair(x1, x2);
    }
    
    throw "Brak miejsc zerowych";
}

double calculate_x1(double a, double b, double delta_root) {
    if(b <= 0) {
        return (-b + delta_root) / (2 * a);
    }
    return (-b - delta_root) / (2 * a);
}

double calculate_x2(double a, double b, double c, double x1) {
    if(x1 != 0){
        return c / (a * x1);
    }
    return -b / a;
}

pair<double, double> new_way(double a, double b, double c) {
    double x1, x2;
    double sqrt_delta = sqrt(b*b - 4*a*c);

    if(sqrt_delta >= 0 && a != 0)
    {
        x1 = calculate_x1(a, b, sqrt_delta);
        x2 = calculate_x2(a, b, c ,x1);

        return make_pair(x1, x2);
    }
    
    throw "Brak miejsc zerowych";
}

int main() {
    double a = 0.000000001;
    double b = 5.0;
    double c = 0.000000001;

    cout << "Dla szkolnej metody" << endl;
    pair<double, double> old_way_result = old_way(a, b, c);
    cout << fixed << setprecision(40) << old_way_result.first << endl;
    cout << old_way_result.second << endl << endl;

    cout << "Dla nowej metody" << endl;
    pair<double, double> new_way_result = new_way(a, b, c);
    cout << new_way_result.first << endl;
    cout << new_way_result.second << endl;

    return 0;
}