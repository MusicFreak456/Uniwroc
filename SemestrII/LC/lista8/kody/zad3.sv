module zad3(input s1,s2,clk,input[3:0]r2,output[3:0]r1);
    always_ff @(posedge clk)
        if (s1) r1 <= r1 + r2;
        else if (s2) r1 <= r1 + 1;
        else r1 <= r1;
endmodule

module upcount_load(
    output logic [3:0] q,
    input [3:0] i,
    input clk, nrst, en, load
);
    always_ff @(posedge clk or negedge nrst)
        if (!nrst) q <= 0;
        else if (load) q <= i;
        else if (en) q <= q + 4'd1;
endmodule

module zad3(
    input s1,s2,clk,nrst
    input[3:0]r2,
    output[3:0]r1
);
    logic [3:0]sum;
    assign sum = r1 + r2;
    upcount_load cnt(r1,sum,clk,nrst,s2,s1);
endmodule