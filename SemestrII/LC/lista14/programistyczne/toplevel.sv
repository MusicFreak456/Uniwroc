
module memory(
    input clk,
    input load,
    input [7:0]datain,
    input [7:0]write_address,
    input [7:0]read_address,
    output signed logic [7:0]out,
);
    integer i;
    signed logic [7:0] mem [255:0];
    /*
    initial 
        for(i = 0; i < 256; i = i + 1)
            mem[i] = 7'b0;*/

    always_ff @(posedge clk)
        if(load) mem[write_address] <= datain;

    assign out = mem[read_address];
endmodule

module brnfck_ctrlpath(
    input clk,
    input nrst,
    input start,
    input in_valid,
    input out_ack,
    output in_ack,
    output out_valid,
    input [2:0] data_signal,
    input [7:0] symbol,
    output [4:0] control_signal,
    output ready
);
    const logic [2:0] READY = 3'b000, ZERO   = 3'b001,
                      WORK  = 3'b010, WRITE  = 3'b011,
                      READ  = 3'b100, LEFT   = 3'b101,
                      RIGHT = 3'b110;
    const logic [7:0] PLUS     = 8'd43, MINUS    = 8'd45,
                      RIGHTS   = 8'd62, LEFTS    = 8'd60,
                      BBRACKET = 8'd91, EBRACKET = 8'd93,
                      DOT      = 8'd46, COMMA    = 8'd44,
                      END      = 8'd0;
    const logic [4:0] SYMBOL_RD = 5'b00000, MHDPP     = 5'b00001,
                      MHDMM     = 5'b00010, HDPP      = 5'b00011,
                      HDMM      = 5'b00100, RESET     = 5'b00101,
                      RDBYTE    = 5'b00110, ZERO_STATE= 5'b00111,
                      NEXT      = 5'b01000, TORIGHT   = 5'b01001,
                      CPPR      = 5'b01010, CMMR      = 5'b01011,
                      TOLEFT    = 5'b01100, CPPL      = 5'b01101,
                      CMML      = 5'b01110, DONOTHING = 5'b01111,
                      PCMM      = 5'b10000;
    logic [3:0]state;

    always_ff @(posedge clk or negedge nrst)
        if(!nrst) begin
            state <= READY;
        end
        else unique case(state)
            READY: if(start) state <= ZERO;
            ZERO: if(data_signal[0]) state <= WORK;
            WORK: begin
                unique case(symbol)
                    DOT: state <= WRITE;
                    COMMA: state <= READ;
                    BBRACKET: if(data_signal[1]) state <= RIGHT;
                              else state <= WORK;
                    EBRACKET: if(data_signal[1]) state <= WORK;
                              else state <= LEFT;
                    END: state <= READY; 
                endcase
            end
            WRITE: if(out_ack) state <= WORK;
            READ: if(in_valid) state <= WORK;
            RIGHT: if(symbol == EBRACKET & !data_signal[2]) state <= WORK;
            LEFT: if(symbol == BBRACKET & !data_signal[2]) state <= WORK;
        endcase

    assign ready = state == READY;

    always_comb begin
        control_signal = DONOTHING;
        out_valid = 0;
        in_ack = 0;

        if(nrst)
        unique case(state)
            READY: begin
                    if(in_valid) begin
                        in_ack = 1;
                        control_signal = SYMBOL_RD;
                    end
            end
            ZERO: control_signal = ZERO_STATE;
            WORK: begin
                unique case(symbol)
                    PLUS: control_signal = MHDPP;
                    MINUS: control_signal = MHDMM;
                    RIGHTS: control_signal = HDPP;
                    LEFTS: control_signal = HDMM;
                    BBRACKET: if(data_signal[1]) control_signal <= TORIGHT;
                              else control_signal <= NEXT;
                    EBRACKET: if(data_signal[1]) control_signal <= NEXT;
                              else control_signal <= TOLEFT;
                    END: control_signal = RESET;
                endcase
            end
            WRITE: begin
                    out_valid = 1;
                    if(out_ack) control_signal = NEXT;
            end
            READ: begin
                  in_ack = 1;
                  if(in_valid) control_signal = RDBYTE;
            end
            RIGHT: begin
                unique case(symbol)
                    BBRACKET: control_signal <= CPPR;
                    EBRACKET: if(data_signal[2]) control_signal <= CMMR;
                              else control_signal <= NEXT;
                    default: control_signal <= NEXT;
                endcase
            end
            LEFT: begin
                unique case(symbol)
                    EBRACKET: control_signal <= CPPL;
                    BBRACKET: if(data_signal[2]) control_signal <= CMML;
                              else control_signal <= NEXT;
                    default: control_signal <= PCMM;
                endcase
            end
        endcase
    end

endmodule

module brnfck_datapath(
    input clk,
    input nrst,
    input [4:0] control_signal,
    input [7:0] in_data,
    output [7:0] symbol,
    output [2:0] data_signal,
    output [7:0] out_data
);
    logic [7:0]hd;
    logic [7:0]pc;
    logic load_txt;
    logic load_mem;
    logic [7:0] text_out;
    logic [7:0] mem_out;
    logic [7:0] mem_value;

    const logic [4:0] SYMBOL_RD = 5'b00000, MHDPP     = 5'b00001,
                      MHDMM     = 5'b00010, HDPP      = 5'b00011,
                      HDMM      = 5'b00100, RESET     = 5'b00101,
                      RDBYTE    = 5'b00110, ZERO_STATE= 5'b00111,
                      NEXT      = 5'b01000, TORIGHT   = 5'b01001,
                      CPPR      = 5'b01010, CMMR      = 5'b01011,
                      TOLEFT    = 5'b01100, CPPL      = 5'b01101,
                      CMML      = 5'b01110, DONOTHING = 5'b01111,
                      PCMM      = 5'b10000;

    always_comb begin
        load_txt = 0;
        load_mem = 0;
        mem_value = 0;

        if(nrst)
        unique case(control_signal)
            SYMBOL_RD: load_txt = 1;
            ZERO_STATE: load_mem = 1;
            MHDPP: begin
                load_mem = 1;
                mem_value = mem_out + 1;
            end
            MHDMM: begin
                load_mem = 1;
                mem_value = mem_out - 1;
            end
            RDBYTE: begin
                load_mem = 1;
                mem_value = in_data;
            end
        endcase
    end

    memory text(clk, load_txt, in_data, pc, pc, text_out);
    memory mem(clk, load_mem, mem_value, hd, hd, mem_out);


    always_ff @(posedge clk or negedge nrst)
        if(!nrst) begin
            pc <= 0;
            hd <= 0;
        end else unique case(control_signal)
            SYMBOL_RD: pc <= pc + 1;
            ZERO_STATE: begin
                hd <= hd + 1;
                pc <= 0;
            end
            MHDPP: pc <= pc + 1;
            MHDMM: pc <= pc + 1;
            HDPP: begin
                hd <= hd + 1;
                pc <= pc + 1;
            end
            HDMM: begin
                hd <= hd - 1;
                pc <= pc + 1;
            end
            NEXT: pc <= pc + 1;
            RESET: begin
                pc <= 0;
                hd <= 0;
            end
            RDBYTE: pc <= pc + 1;
            TORIGHT: begin
                c <= 0;
                pc <= pc + 1;
            end
            TOLEFT: begin
                c <= 0;
                pc <= pc - 1;
            end
            CPPR: begin
                c <= c + 1;
                pc <= pc + 1;
            end
            CMMR: begin
                c <= c - 1;
                pc <= pc + 1;
            end
            CPPL: begin
                c <= c + 1;
                pc <= pc - 1;
            end
            CMML: begin
                c <= c - 1;
                pc <= pc - 1;
            end
            PCMM: pc <= pc - 1;
        endcase

    logic [7:0] hdp1 = hd + 1;
    assign data_signal[0] = hdp1 == 8'd0;
    assign data_signal[1] = mem_out == 0;
    assign data_signal[2] = c > 0;
    assign symbol = text_out;
    assign out_data = mem_out;
endmodule

module boje_sie_spojrzec_na_nastepna_liste(
    input clk,
    input nrst,
    input [7:0] in_data,
    input in_valid,
    output in_ack,
    output [7:0] out_data,
    output out_valid,
    input out_ack,
    input start,
    output ready
);
    logic [7:0] symbol;
    logic [2:0] data_signal;
    logic [4:0] control_signal;
    brnfck_ctrlpath ctrl_path(clk, nrst, start, in_valid, out_ack, in_ack, out_valid, data_signal, symbol, control_signal, ready);
    brnfck_datapath dt_path(clk, nrst, control_signal, in_data, symbol, data_signal, out_data);

endmodule