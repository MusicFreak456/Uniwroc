module z4(input clk,ra,rb,output e)
    always_latch if (clk) begin
        rb = ra - 1;
        if (rb == 0) e = 1;
        else e = 0;
    end
endmodule
