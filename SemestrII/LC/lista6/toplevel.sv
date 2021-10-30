module d_latch (input en,d, output q, nq);
    logic lt;
    logic lb;
    nand left_top(lt,d,en);
    nand left_bottom(lb,lt,en);
    nand right_top(q,lt,nq);
    nand right_bottom(nq,q,lb);
endmodule

module flipy_floppy (input c,d, output q,nq);
    logic q1;
    d_latch inst_1(!c,d, q1, );
    d_latch inst_2(c,q1, q,nq);
endmodule

module mini_shifter (input left_bit,middle_bit,right_bit,l,r,both,c, output result_bit);
    logic mids,lefts,rights;
    logic or1;
    logic res1;
    and left(lefts,l,left_bit);
    and mid(mids,both,middle_bit);
    and right(rights,r,right_bit);
    or o1(or1,lefts,rights);
    or o2(res1,or1,mids);

    flipy_floppy ff(c,res1,result_bit,);
endmodule

module uni_shifter(output [7:0] q, input [7:0] d, input i,c,l,r);
    logic clock_control;
    assign clock_control = (l|r) & c;
    logic f1o,f2o,f3o,f4o,f5o,f6o,f7o,f8o;

    logic both;
    assign both = r&l;
    logic nl;
    logic nr;
    assign nl = !l;
    assign nr = !r;
    mini_shifter ms1(i,   d[7],q[6],nl,nr,both,clock_control, q[7]);
    mini_shifter ms2(q[7],d[6],q[5],nl,nr,both,clock_control, q[6]);
    mini_shifter ms3(q[6],d[5],q[4],nl,nr,both,clock_control, q[5]);
    mini_shifter ms4(q[5],d[4],q[3],nl,nr,both,clock_control, q[4]);
    mini_shifter ms5(q[4],d[3],q[2],nl,nr,both,clock_control, q[3]);
    mini_shifter ms6(q[3],d[2],q[1],nl,nr,both,clock_control, q[2]);
    mini_shifter ms7(q[2],d[1],q[0],nl,nr,both,clock_control, q[1]);
    mini_shifter ms8(q[1],d[0],i,nl,nr,both,clock_control, q[0]);
endmodule