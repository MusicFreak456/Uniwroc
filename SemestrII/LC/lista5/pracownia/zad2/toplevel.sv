module grey_to_bin(input[31:0]i ,output[31:0]o);
    integer res;
    integer k;
    always_comb begin
        res=i;
        for(k=1;k<=32;k=k+k) begin
            res = res ^ (res>>k);
        end
        o=res;
    end

endmodule