module counter(input[15:0]d, input[1:0]sel,input clk,nrst, output logic [15:0]cnt);
    always_ff @(posedge clk)
    if(sel == 2'd3)
    cnt<=d;
    else if(!nrst)
    cnt<=1'b0;
    else 
    cnt<=cnt+16'd1;

endmodule

module toper(input[15:0]d, input[1:0]sel,input clk,output logic [15:0]top);
    always_ff@(posedge clk)
    if(sel== 2'd2)
    top<=d;
endmodule

module compare(input[15:0]d, input[1:0]sel,input clk,output logic [15:0]top);
    always_ff@(posedge clk)
    if(sel== 2'd1)
    top<=d;
endmodule



module pwm(input clk, input[15:0]d, input[1:0]sel,output[15:0]cnt,output[15:0]cmp,output[15:0]top,output out);
    logic [15:0]cnttemp;
    logic [15:0]cmptemp;
    logic [15:0]toptemp;

    logic nrst;
    assign nrst = toptemp>cnttemp;
    
    counter cn(d,sel,clk,nrst,cnttemp);
    toper tp(d,sel,clk,toptemp);
    compare cp(d,sel,clk,cmptemp);

    assign cnt = cnttemp;
    assign top = toptemp;
    assign cmp = cmptemp;
    assign out = cmptemp>cnttemp;
endmodule

