module nwd_controlpath(
    input clk,
    input nrst,
    input start,
    input eq,
    input lesser,
    output [1:0] control_signal
);
    const logic READY = 1'b0, BUSY = 1'b1;
    const logic [1:0] INIT = 2'b01, SWAP = 2'b10,
                      EQUAL = 2'b00, SUB = 2'b11;
    logic state;

    always_ff @(posedge clk or negedge nrst)
        if(!nrst)
            state <= READY;
        else case (state)
            READY: if(start) state <= BUSY;
            BUSY: if(eq) state <= READY;
        endcase

    always_comb begin
        control_signal = 2'b00;

        case (state)
            READY: begin
                if(start) control_signal = INIT;
            end
            BUSY: if(eq) control_signal = EQUAL;
                else if (lesser) control_signal = SWAP;
                else control_signal = SUB;
        endcase
    end

endmodule

module nwd_datapath(
    input clk,
    input nrst,
    input [1:0] control_signal,
    input [7:0] ina,
    input [7:0] inb,
    output eq,
    output lesser,
    output [7:0] out,
    output logic ready
);
    const logic [1:0] INIT = 2'b01, SWAP = 2'b10,
                      EQUAL = 2'b00, SUB = 2'b11;

    logic [7:0] a;
    logic [7:0] b;

    always_ff @(posedge clk or negedge nrst)
        if(!nrst) begin
            a <= 8'b0;
            b <= 8'b0;
            ready <= 1'b1;
        end
        else case (control_signal)
            INIT: begin
                a <= ina;
                b <= inb;
                ready <= 1'b0;
            end
            SWAP: begin
                a <= b;
                b <= a;
            end
            SUB: begin
                a <= a - b;
            end
            EQUAL: begin
                ready <= 1'b1;
            end
        endcase

    assign out = a;
    assign eq = a == b;
    assign lesser = a < b;

endmodule


module nwd(
    input clk,
    input nrst,
    input start,
    input [7:0] ina,
    input [7:0] inb,
    output ready,
    output [7:0] out
);
    logic eq, lesser;
    logic [1:0]control_signal;

    nwd_controlpath controlpath(clk, nrst, start, eq, lesser, control_signal);
    nwd_datapath datapath(clk, nrst, control_signal, ina, inb, eq, lesser, out, ready );

endmodule