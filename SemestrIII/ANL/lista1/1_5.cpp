#include<iostream>
#include<cstdlib>
#include<cmath>

using namespace std;

double series_element(int index) {
    return pow(-1, index) / ( 2.0 * index + 1.0 );
}

int main() {
    double result = 0.0;

    for(int i = 0; i <= 20'000; i++ ) {
        result += series_element(i); 
    }

    cout << fixed << result * 4.0 - M_PI << endl;

    result = 0.0;

    for(int i = 0; i <= 20'000; i++ ) {
        result += series_element(i); 
        if( abs(result * 4.0 - M_PI) < 0.0001 ){
            cout << i << endl;
            break;
        }
    }


    return 0;
}