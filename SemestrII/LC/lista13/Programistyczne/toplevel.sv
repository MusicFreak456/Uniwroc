module calc_pointer(
    input clk,
    input en,
    input nrst,
    input next,
    input prev,
    output logic [9:0]out
);
    always_ff @(posedge clk, negedge nrst)
        if(!nrst) out <= 10'b0;
        else if(en) begin
            if(next) out <= out + 1;
            else if(prev && (out > 0)) out <= out - 1;
        end
endmodule

module top_register(
    input clk,
    input en,
    input nrst,
    input signed [15:0]d,
    output signed logic [15:0]out
);
    always_ff @(posedge clk, negedge nrst)
        if(!nrst) out <= 16'b0;
        else if(en) out <= d;
endmodule

module memory(
    input clk,
    input en,
    input load,
    input signed [15:0]datain,
    input [9:0]write_address,
    input [9:0]read_address,
    output signed logic [15:0]out,
);
    integer i;
    signed logic [15:0] mem [1023:0];
    initial 
        for(i = 0; i < 1024; i = i + 1)
            mem[i] = 16'b0;

    always_ff @(posedge clk)
        if(load & en) mem[write_address] <= datain;

    assign out = mem[read_address];
endmodule

module operate(
    input signed [15:0]a,
    input signed [15:0]b,
    input [2:0]op,
    output signed [15:0]res
);
    const logic [2:0] GREATER = 3'b000, NEG = 3'b001,
                      ADD  = 3'b010, MUL = 3'b011,
                      SWAP = 3'b100, LOAD = 3'b101;

    always_comb begin
        unique casez (op)
            GREATER : begin 
                if(a > 0) res = 1;
                else res = 0;
            end
            NEG : res = -a;
            ADD : res = a + b;
            MUL : res = a * b;
            3'b11? : res = b;
            SWAP : res = b;
            LOAD : res = b;
            default: res = a;
        endcase
    end

endmodule

module nwd_calc_control(
    input [2:0] op,
    input [9:0] address_out,
    input push,
    input [15:0] top,
    output [9:0] read_address,
    output [9:0] write_address,
    output load,
    output pop
);
    const logic [2:0] ADD  = 3'b010, MUL = 3'b011,
                      SWAP = 3'b100, LOAD = 3'b101;

    always_comb begin
        pop = 0;
        load = push;
        write_address = address_out - 1;
        read_address = address_out - 2;
        if(!push)
        unique casez(op)
            3'b11?: pop = 1;
            ADD: pop = 1;
            MUL: pop = 1;
            SWAP: begin
                load = 1;
                write_address = address_out - 2;
            end
            LOAD: begin
                read_address = address_out - 2 - top;
            end
        endcase
    end

endmodule

module nwd_calc(
    input nrst,
    input step,
    input en,
    input signed [15:0]d,
    input push,
    input [2:0]op,
    output signed [15:0]out,
    output [9:0]cnt
);
    logic [15:0] top_out;
    logic [15:0] sec_out;
    logic [9:0] address_out;
    logic [9:0] read_address;
    logic [9:0] write_address;
    logic [15:0] res;
    logic pop;
    logic load;
    
    nwd_calc_control ncc(op, address_out, push, top_out, read_address, write_address, load, pop);
    operate ope(top_out, sec_out, op, res);
    calc_pointer cn(step, en, nrst, push, pop, address_out);
  	top_register tr(step, en, nrst, push? d : res, top_out);
    memory m(step, en, load, top_out, write_address, read_address, sec_out);

    assign out = top_out;
    assign cnt = address_out;

endmodule

module pcal_ctrlpth(
    input clk,
    input nrst,
    input start,
    input wr,
    input [2:0] data_signal,
    output [2:0] control_signal
);
    const logic READY = 1'b1, BUSY = 1'b0;
    const logic [2:0] WRITE = 3'b000, INIT   = 3'b001,
                      ENOFF = 3'b010, LOAD15 = 3'b011,
                      JUMP  = 3'b100, NEXTOP = 3'b110,
                      DONOTHING = 3'b111;
    logic state;

    always_ff @(posedge clk or negedge nrst)
        if(!nrst)
            state <= READY;
        else unique case(state)
            READY: if(start) state <= BUSY;
            BUSY: if(data_signal[2] & data_signal[1]) state <= READY;
        endcase
    
    always_comb begin
        control_signal = DONOTHING;

        unique case(state)
            READY:  if(start) control_signal = INIT;
                    else if(wr) control_signal = WRITE;
                    else control_signal = DONOTHING;
            BUSY:   if(data_signal[2]) begin
                        if(data_signal[1]) control_signal = ENOFF;
                        else begin 
                            if(data_signal[0]) control_signal = JUMP;
                            else control_signal = NEXTOP;
                        end
                    end
                    else control_signal = LOAD15;
        endcase
    end
endmodule

module pcal_dtpth(
    input clk,
    input nrst,
    input [2:0] control_signal,
    input [9:0] addr,
    input [15:0] datain,
    output [15:0] out,
    output logic ready,
    output [2:0] data_signal
);
    const logic [2:0] WRITE = 3'b000, INIT   = 3'b001,
                      ENOFF = 3'b010, LOAD15 = 3'b011,
                      JUMP  = 3'b100, NEXTOP = 3'b110,
                      DONOTHING = 3'b111;
                    
    logic load;
    logic [15:0] mem_out;

    logic [9:0] pc;
    logic en;
    logic [2:0] op;
    logic push;
    logic [15:0]d;

    assign load = nrst & (control_signal == WRITE);

    always_comb begin
        en = 0;
        push = 0;

        if(nrst)
        unique case (control_signal)
            ENOFF: en = 0;
            LOAD15: begin
                en = 1;
                push = 1;
            end
            JUMP: begin
                en = 1;
                push = 0;
            end
            NEXTOP: begin
                en = 1;
                push = 0;
            end
        endcase
    end

    memory prog_mem(clk, 1, load, datain, addr, pc, mem_out);

    always_ff @(posedge clk or negedge nrst)
        if(!nrst) begin
            pc <= 10'd0;
            ready <= 1;
        end else unique case(control_signal)
            INIT: begin
                pc <= 10'd0;
                ready <= 0;
            end
            ENOFF: begin
                ready <= 1;
            end
            LOAD15: begin
                pc <= pc + 1;
            end
            JUMP: begin
                pc <= out;
            end
            NEXTOP: begin
                pc <= pc + 1;
            end
        endcase

    assign op = mem_out[2:0];
    assign d = mem_out;

    nwd_calc calc(nrst, clk, en, d, push, op, out,);

    assign data_signal[2] = mem_out[15];
    assign data_signal[1] = mem_out[14];
    assign data_signal[0] = mem_out[2:0] == 3'd7;
endmodule

module kalk___ator(
    input clk,
    input nrst,
    input [9:0] addr,
    input wr,
    input [15:0] datain,
    input start,
    output ready,
    output [15:0] out
);
    logic [2:0] control_signal;
    logic [2:0] data_signal;
    pcal_ctrlpth pcal_ctrlpth(clk, nrst, start, wr, data_signal, control_signal);
    pcal_dtpth pcal_dtpth(clk, nrst, control_signal, addr,datain, out, ready, data_signal);
endmodule