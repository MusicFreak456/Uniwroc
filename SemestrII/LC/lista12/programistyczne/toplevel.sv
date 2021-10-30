module memory(
    input clk,
    input write,
    input [7:0] datain,
    input [2:0] adress,
    output logic [7:0] dataout
);
    integer i;
    signed logic [7:0] mem [7:0];
    initial 
        for(i = 0; i < 8; i = i + 1)
            mem[i] = 8'b0;

    always_ff @(posedge clk)
        if(write) mem[adress] <= datain;
        else dataout <= mem[adress];
endmodule



module sort_controlpath(
    input clk,
    input nrst,
    input start,
    input wr,
    input [3:0] data_signal,
    output [3:0] control_signal
);
    const logic [2:0] READY = 3'b000, OUTER = 3'b001,
                      INNER = 3'b010, END   = 3'b011,
                      SWAP  = 3'b100, TEMP  = 3'b101;
    const logic [3:0] INIT   = 4'b0000, READ    = 4'b0001,
                      WRITE  = 4'b0010, TOINNER = 4'b0011,
                      CLTM0  = 4'b0100, INLOOP  = 4'b0101,
                      TOEND  = 4'b0110, OUTLOOP = 4'b0111,
                      TOSWAP = 4'b1000, TOOUT   = 4'b1001,
                      DONOTHING = 4'b1111, CLTM1= 4'b1011;
    
    logic [2:0]state;

    always_ff @(posedge clk or negedge nrst)
        if(!nrst)
            state <= READY;
        else unique case (state)
            READY: if(start) state <= OUTER;
            OUTER: if(data_signal[3]) state <= READY;
                   else state <= INNER;
            INNER: if(data_signal[2]) state <= END;
            END: if(data_signal[0]) state <= OUTER;
                 else state <= SWAP;
            SWAP: state <= TEMP;
            TEMP: state <= OUTER; 
        endcase

    always_comb begin
        control_signal = DONOTHING;

        unique case (state)
            READY: if(start) control_signal = INIT;
                   else if(wr) control_signal = WRITE;
                   else control_signal = READ;
            OUTER: if(!data_signal[3]) control_signal = TOINNER;
            INNER: begin
                    if(data_signal[1]) begin
                        if(data_signal[2]) control_signal = CLTM1;
                        else control_signal = CLTM0;
                    end else
                        if(data_signal[2]) control_signal = TOEND;
                        else control_signal = INLOOP;
            end
            END: if(data_signal[0]) control_signal = OUTLOOP;
                 else control_signal = TOSWAP;
            SWAP: control_signal = TOOUT;
            TEMP: control_signal = OUTLOOP;
        endcase
    end
endmodule

module sort_datapath(
    input clk,
    input nrst,
    input [2:0] addr,
    input [7:0] datain,
    input [3:0] control_signal,
    output [3:0] data_signal,
    output [7:0] dataout,
    output logic ready
);
    const logic [3:0] INIT   = 4'b0000, READ    = 4'b0001,
                      WRITE  = 4'b0010, TOINNER = 4'b0011,
                      CLTM0  = 4'b0100, INLOOP  = 4'b0101,
                      TOEND  = 4'b0110, OUTLOOP = 4'b0111,
                      TOSWAP = 4'b1000, TOOUT   = 4'b1001,
                      DONOTHING = 4'b1111, CLTM1= 4'b1011;

    logic [2:0] i;
    logic [2:0] j;
    logic [2:0] jm;
    logic [7:0] c;
    logic [7:0] m;
    logic [2:0] address;
    logic [7:0] value;
    logic write;

    always_comb begin
        address = 3'b000;
        value = datain;
        write = 0;

        if(nrst)
        unique case (control_signal)
            READ: begin
                address = addr;
            end
            WRITE: begin
                address = addr;
                write = 1;
            end
            TOINNER: address = i + 1;
            CLTM0: address = j + 1;
            CLTM1: address = i;
            INLOOP: address = j + 1;
            TOEND: address = i;
            OUTLOOP: address = i + 1;
            TOSWAP: begin
                address = jm;
                write = 1;
                value = c;
            end
            TOOUT: begin
                address = i;
                write = 1;
                value = m;
            end

        endcase
    end

    memory mem(clk, write, value, address, dataout);

    always_ff @(posedge clk or negedge nrst)
        if(!nrst) begin
            i <= 3'b000;
            j <= 3'b000;
            jm <= 3'b000;
            m <= 7'b0000000;
            ready <= 1'b1;
        end
        else unique case (control_signal)
            INIT: begin
                i <= 3'b000;
                ready <= 1'b0;
            end
            TOINNER: begin
                j <= i + 1;
                jm <= i;
                m <= c;
            end
            CLTM0: begin
                m <= c;
                jm <= j;
                j <= j + 1;
            end
            CLTM1: begin
                m <= c;
                jm <= j;
            end
            INLOOP: begin
                j <= j + 1;
            end
            OUTLOOP: begin
                i <= i + 1;
            end
            DONOTHING: begin
                ready = 1'b1;
            end
        endcase

    assign c = dataout;
    assign data_signal[3] = i == 3'd7;
    assign data_signal[2] = j == 3'd7;
    assign data_signal[1] = c < m;
    assign data_signal[0] = i == jm;
endmodule

module sort_out_my_life_pls(
    input clk,
    input nrst,
    input start,
    input [2:0]addr,
    input wr,
    input [7:0] datain,
    output [7:0] dataout,
    output ready
);
    logic [3:0] control_signal;
    logic [3:0] data_signal;
    sort_controlpath sctl(clk, nrst, start, wr, data_signal, control_signal);
    sort_datapath sdt(clk, nrst, addr, datain, control_signal, data_signal, dataout, ready);
endmodule