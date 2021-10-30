module zad1(input[7:0] i ,input[4:0] choice, output [7:0] o );
    always_comb begin
        case(choice)
            'b00010: o = i;
            'b00100: o = i + i;
            'b01000: o = i + i + i;
            default: o = i + i + i +i;
        endcase
    end
endmodule
