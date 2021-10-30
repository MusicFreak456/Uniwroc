
module predC8(input G0,G1,P1, output c8);
    assign c8 = (G0 && P1) || G1;
endmodule
module predC12(input G0,G1,G2,P1,P2, output c12);
    assign c12 = (G0 && P1&& P2) || (G1 && P2) || G2;
endmodule

module block(input [3:0]a, input[3:0]b ,input [0:0] c0 ,output [0:0]G,output[0:0] P,output[3:0]S);//nie podanie [0:0] sprawiało że pojawiały się moduły zero-extend i zwiększała się ścieżka 

    assign g0 = a[0] && b[0];
    assign g1 = a[1] && b[1];
    assign g2 = a[2] && b[2];
    assign g3 = a[3] && b[3];

    assign p0 = a[0] || b[0];
    assign p1 = a[1] || b[1];
    assign p2 = a[2] || b[2];
    assign p3 = a[3] || b[3];
    
    assign c1 = c0 & p0 | g0;
    assign c2 = c0 & p0 & p1 | g0 & p1 | g1;
    assign c3 = c0 & p0 & p1 & p2 | g0 & p1 & p2 | g1 & p2 | g2;

    assign S[0] = a[0] ^ b[0] ^ c0;
    assign S[1] = a[1] ^ b[1] ^ c1;
    assign S[2] = a[2] ^ b[2] ^ c2;
    assign S[3] = a[3] ^ b[3] ^ c3;

    assign G = g0 && p1 && p2 && p3 || g1 && p2 && p3 || g2 && p3 || g3;
    assign P = p0 && p1 && p2 && p3;
endmodule

module toplevel (input[15:0]a, input[15:0]b ,output[15:0]o);
    logic C4,C8,C12;
    logic G0,G1,G2,G3;
    logic P0,P1,P2,P3;

    block B0(a[3:0] ,b[3:0] , 0 , G0 , P0, o[3:0]);

    block B1(a[7:4] ,b[7:4] , G0 , G1 , P1, o[7:4]);
    predC8 predc8(G0,G1,P1,C8);

    block B2(a[11:8] ,b[11:8] , C8 , G2 , P2, o[11:8]);
    predC12 predc12(G0,G1,G2,P1,P2,C12);

    block B3(a[15:12] ,b[15:12] , C12 , G3 , P3, o[15:12]);
endmodule