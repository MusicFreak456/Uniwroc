
//tff pożyczony z wykładu
module tff(output q, nq, input t, clk, nrst);
    logic ns, nr, ns1, nr1, j, k;
    nand n1(ns, clk, j), n2(nr, clk, k),
    n3(q, ns, nq), n4(nq, nr, q, nrst),
    n5(ns1, !clk, t, nq), n6(nr1, !clk, t, q),
    n7(j, ns1, k), n8(k, nr1, j, nrst);
endmodule

module syncnt(output [3:0] out, input step, down, clk, nrst);
    
    logic nq[3:0];
    logic and1, and2, and3;

    assign and1 = (!step & (down? nq[0]: out[0])) | step;
    and a2 (and2 , and1, down? nq[1]: out[1] );
    and a3 (and3 , and2, down? nq[2]: out[2] );

    tff t1(out[0],nq[0], !step ,clk , nrst);
    tff t2(out[1],nq[1],and1 ,clk , nrst);
    tff t3(out[2],nq[2],and2 ,clk , nrst);
    tff t4(out[3],nq[3],and3 ,clk , nrst);
endmodule