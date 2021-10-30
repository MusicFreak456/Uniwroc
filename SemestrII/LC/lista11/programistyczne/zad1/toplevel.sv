module quick_pow(
    input clk,
    input nrst,
    input start,
    input [15:0] inx,
    input [7:0] inn,
    output ready,
    output [15:0] out
);
    const logic READY = 1'b0, BUSY = 1'b1;
    logic state;
    logic [15:0] a;
    logic [7:0] n;
    logic [15:0] x;
    logic even_n;
    logic [15:0]to_mult;
    logic [15:0]mult_res;

    assign even_n = n[0] == 0;
    assign to_mult = even_n? x : a; 
    assign mult_res = to_mult * x;

    always_ff @(posedge clk or negedge nrst) begin
        if(!nrst) begin
            state <= READY;
            ready <= 1;
            out <= 16'b0;
        end else case (state)
            READY: if(start) begin
                state <= BUSY;
                ready <= 1'b0;
                a <= 16'd1;
                x <= inx;
                n <= inn;
            end
            BUSY: if(n == 0) begin
                state <= READY;
                ready <= 1;
                out <= a;
                end
                else begin
                    if(even_n) begin
                        x <= mult_res;
                        n <= n >> 1;
                    end
                    else begin
                        a <= mult_res;
                        n <= n - 1;
                    end
                end
        endcase
    end
        

endmodule