module dff(output q, input clk, d);
    logic r, s, nr, ns, q;
    nand gq(q, nr, nq), gnq(nq, ns, q),
    gr(nr, clk, r), gs(ns, nr, clk, s),
    gr1(r, nr, s), gs1(s, ns, d);
endmodule

module syncnt_load(output [3:0] q, input [3:0] d,input en, clk, load);
    genvar n;

    logic [3:0] t;

    assign t = {q[2] & t[2], q[1] & t[1], q[0] & t[0], en};

    for (n = 0; n < 4; n = n + 1)
    dff df(q[n], clk, load ? d[n] : q[n] ^ t[n]);
endmodule 

module modulo6_sync(output [3:0] q,output o3, input clk, rst, start);
    logic o1,o2;
    logic en;
    assign en = start | o3;
    or a(o1,q[0],q[1]);
    or b(o2,q[2],q[3]);
    or c(o3,o1,o2);
    syncnt_load sc(q, 4'd0, en, clk,q[2] && q[3] || rst);
endmodule