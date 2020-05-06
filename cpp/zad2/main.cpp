#include<iostream>
#include<cstdlib>
#include<cmath>

#include"plain.hpp"

using namespace std;

int main()
{
    Wektor test;
    Wektor v1(1,1);
    Wektor v2(v1);

    cout << "v1: ";
    v1.print();
    cout << "v2: "; 
    v2.print();

    Wektor v3(dodaj_wektory(v1,v2));
    cout << "v3(v1 + v2): ";
    v3.print();

    Punkt p1(0,0);
    cout << "p1: ";
    p1.print();

    Punkt p2(p1,v3);
    cout << "p2: ";
    p2.print();

    Punkt p3(1,2);
    cout << "p3: ";
    p3.print();

    Prosta l(p1,p3);
    cout << "l(p1,p3): ";
    l.print();

    Punkt p4(-1,2);
    cout << "p4: ";
    p4.print();

    Punkt p5(0,1);
    cout << "p5: ";
    p5.print();

    Prosta k(p4,p5);
    cout << "k: ";
    k.print();

    Prosta m(4,-2,2);
    cout << "m: ";
    m.print();
    
    Prosta z(1,5,0);
    cout << "z: ";
    z.print();
    cout << "Normalized z: ";
    z.print_normalized();

    Wektor line_constr(1,1);
    cout << "line_constr: ";
    line_constr.print();

    Prosta vector_line(line_constr);
    cout << "vector_line: ";
    vector_line.print();

    Prosta to_be_moved(-2,1,1);
    cout << "to_be_moved: ";
    to_be_moved.print();

    Wektor move(2,0);
    cout << "move: ";
    move.print();

    Prosta moved_line(&to_be_moved,move);
    cout << "moved_line: ";
    moved_line.print();

    Punkt p6(2.5,0);
    cout << "p6: ";
    p6.print();

    cout << "dist from moved line to p6: " << moved_line.odleglosc(p6) << endl;

    try
    {
        Prosta try_test(0,0,1);
    }
    catch(const std::invalid_argument& e)
    {
        std::cerr << e.what() << '\n';
    }
    

    Prosta perp_pal_check(-1,-1,1);
    cout << "per_pel_check: ";
    perp_pal_check.print();

    Wektor perp_check(0.5,0.5);
    cout << "perp_check: ";
    perp_check.print();

    cout << "Is perp_check perpenpendicular to per_pel_check: " << (perp_pal_check.czy_prostopadla(perp_check)? "Yes" : "No") << endl;
    cout << "Is move perperpendicular to per_pel_check: " << (perp_pal_check.czy_prostopadla(move)? "Yes" : "No") << endl;

    Wektor parall_check(1,-1);
    cout << "Is perp_check perpenpendicular to per_pel_check: " << (perp_pal_check.czy_rownolegla(parall_check)? "Yes" : "No") << endl;
    cout << "Is move perpenpendicular to per_pel_check: " << (perp_pal_check.czy_rownolegla(move)? "Yes" : "No") << endl;

    cout << "Is p1 in l: " << (l.czy_nalezy(p1)? "Yes" : "No") << endl;
    cout << "Is p2 in l: " << (l.czy_nalezy(p2)? "Yes" : "No") << endl;

    Prosta perp(1,-1,0);
    cout << "perp: ";
    perp.print();

    cout << "Is perp perpendicular to perp_pal_check: " << (czy_prostopadle(&perp_pal_check,&perp)? "Yes" : "No") <<endl;
    cout << "Is moved_line perpendicular to perp_pal_check: " << (czy_prostopadle(&perp_pal_check,&moved_line)? "Yes" : "No") <<endl;

    Prosta par(-1,-1,0);
    cout << "parr: ";
    par.print();

    cout << "Is par parallel to perp_pal_check: " << (czy_rownolegle(&perp_pal_check,&par)? "Yes" : "No") <<endl;
    cout << "Is moved_line parallel to perp_pal_check: " << (czy_rownolegle(&perp_pal_check,&moved_line)? "Yes" : "No") <<endl;

    Prosta to_be_solved_a(2,-1,1);
    cout << "to_be_solved_a: ";
    to_be_solved_a.print();

    Prosta to_be_solved_b(3,-1,2);
    cout << "to_be_solved_b: ";
    to_be_solved_b.print();

    cout << "Solution: "<< rozwiazanie(&to_be_solved_a,&to_be_solved_b) << endl;




    return 0;
}