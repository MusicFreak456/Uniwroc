module circuit(input [0:3]i, output [0:3]d);
  assign d[0] = !i[0] & !i[1] & !i[2];
  assign d[1] = !i[1] & i[2] || i[1] & !i[2];
  assign d[2] = i[2];
  assign d[3] = !i[3];
endmodule