module tff(output q, nq, input t, clk, nrst);
    logic ns, nr, ns1, nr1, j, k;
    nand n1(ns, clk, j), n2(nr, clk, k),
    n3(q, ns, nq), n4(nq, nr, q, nrst),
    n5(ns1, !clk, t, nq), n6(nr1, !clk, t, q),
    n7(j, ns1, k), n8(k, nr1, j, nrst);
endmodule

module syncnt(output [2:0] q, input[2:0]i, input en,r, clk, nrst);
    
    logic nq[2:0];
    logic an1, an2;

    and a1 (an1 , en , q[0] );
    and a2 (an2 , an1, q[1] );

    tff t1(q[0],nq[0],r? (q[0] ^ i[0]): en ,clk , nrst);
    tff t2(q[1],nq[1],r? (q[1] ^ i[1]):an1 ,clk , nrst);
    tff t3(q[2],nq[2],r? (q[2] ^ i[2]):an2 ,clk , nrst);
endmodule