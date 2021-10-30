module adress_pointer(
    input clk,
    input nrst,
    input next,
    input prev,
    output logic [9:0]out
);
    always_ff @(posedge clk, negedge nrst)
        if(!nrst) out <= 10'b0;
        else if(next) out <= out + 1;
        else if(prev && (out > 0)) out <= out - 1;
endmodule

module top_register(
    input clk,
    input nrst,
    input signed [15:0]d,
    output signed logic [15:0]out
);
    always_ff @(posedge clk, negedge nrst)
        if(!nrst) out <= 16'b0;
        else out <= d;
endmodule

module memory(
    input clk,
    input load,
    input signed [15:0]d,
    input [9:0]adress,
    output signed logic [15:0]top,
);
    integer i;
    signed logic [15:0] mem [1023:0];
    initial 
        for(i = 0; i < 1024; i = i + 1)
            mem[i] = 16'b0;

    always_ff @(posedge clk)
        if(load) mem[adress-1] <= d;

    assign top = mem[adress-2];
endmodule

module operate(
    input signed [15:0]a,
    input signed [15:0]b,
    input [1:0]op,
    output signed [15:0]res
);
    const logic [1:0] NONE = 2'b00, NEG = 2'b01,
                      ADD  = 2'b10, MUL = 2'b11;

    always_comb begin
        res = 15'd0;
        unique case (op)
            NONE : res = a;
            NEG : res = -a;
            ADD : res = a + b;
            MUL : res = a * b;
        endcase
    end

endmodule

module nwd_calc(
    input nrst,
    input step,
    input signed [15:0]d,
    input push,
    input [1:0]op,
    output signed [15:0]out,
    output [9:0]cnt
);
    logic [15:0] top_out;
    logic [15:0] sec_out;
    logic [9:0]  adress_out;
    logic [15:0] res;

    operate ope(top_out, sec_out, op, res);
    adress_pointer cn(step, nrst, push,(op != 0 && op != 1), adress_out);
  	top_register tr(step, nrst, push? d : res, top_out);
    memory m(step, push, top_out, adress_out, sec_out);

    assign out = top_out;
    assign cnt = adress_out;

endmodule