module przesuwator(input[3:0]i,input l, r, output[3:0]o);
  assign o[3] = i[2]&l | (r~|l) & i[3];
  assign o[2] = i[1]&l | i[3] &r | (r~|l) & i[2];
  assign o[1] = i[0]&l | i[2] &r | (r~|l) & i[1];
  assign o[0] = i[1]&r | (r~|l) & i[0];
endmodule