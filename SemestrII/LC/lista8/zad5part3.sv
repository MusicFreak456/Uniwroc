
module swap (
    input [15:0]n,
    input [15:0]m,
    output[15:0]nout,
    output[15:0]mout
);
  logic bign = n > m;
  assign nout = bign? n : m;
  assign mout = bign? m : n;  
endmodule

module nwd(
    input [15:0]n,
    input [15:0]m,
    input clk,
    input ini,
    output [15:0]nwd,
    output fin
);
logic [15:0]subres,accm;
logic [15:0]newn,newm;
logic [15:0]initn,initm;

assign initm = ini? m : accm;
assign initn = ini? n : subres;

swap sw(initn,initm,newn,newm);
assign fin = newm==newn;

always_ff @(posedge clk) begin
    subres<=fin? newn : newn-newm;
    accm<=newm;
end

assign nwd = accm;
endmodule