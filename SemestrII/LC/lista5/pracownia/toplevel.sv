module switchplace (input[7:0]i ,output[7:0] o);
    logic [3:0]a = i[7:4];
    logic [3:0]b = i[3:0];
    assign o = (a < b)? {b,a} : {a,b};
endmodule

module merge (input[7:0]sorted_pair_a, input[7:0]sorted_pair_b, output[15:0]o );
    logic [3:0]bigger_a = sorted_pair_a[7:4];
    logic [3:0]lesser_a = sorted_pair_a[3:0];
    logic [3:0]bigger_b = sorted_pair_b[7:4];
    logic [3:0]lesser_b = sorted_pair_b[3:0];
    logic big_a = bigger_a > bigger_b;
    logic less_a = lesser_a < lesser_b;
    assign o[15:12] = (big_a)? bigger_a: bigger_b;
    assign o[3:0] = (less_a)? lesser_a: lesser_b;
    logic [3:0]to_be_sorted_a;
    logic [3:0]to_be_sorted_b;

    always_comb begin
        if(big_a & less_a) begin
        to_be_sorted_a = bigger_b;
        to_be_sorted_b =lesser_b;
        end
        else if (big_a ~| less_a) begin
        to_be_sorted_a = bigger_a;
        to_be_sorted_b =lesser_a;
        end
        else if (big_a) begin
        to_be_sorted_a = bigger_b;
        to_be_sorted_b =lesser_a;
        end
        else begin
        to_be_sorted_a = bigger_a;
        to_be_sorted_b =lesser_b;
        end
    end

    switchplace decide({to_be_sorted_a , to_be_sorted_b} , o[11:4]);
endmodule

module sortowanko (input[15:0]i, output[15:0]o);
    logic [7:0]sorted_pair_a;
    logic [7:0]sorted_pair_b;
    switchplace a( i[15:8], sorted_pair_a);
    switchplace b( i[7:0], sorted_pair_b);
    merge m(sorted_pair_a, sorted_pair_b, o);
endmodule