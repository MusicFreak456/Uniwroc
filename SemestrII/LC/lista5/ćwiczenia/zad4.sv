module decoder_2_to_4(
    output [3:0] o,
    input [1:0] i); //z wykładu
    always_comb 
        case(i)
            2'd0: o = 4'b0001;
            2'd1: o = 4'b0010;
            2'd2: o = 4'b0100;
            2'd3: o = 4'b1000;
        endcase
endmodule


//Mam dużo pytań
module decoder_3_to_8(input [2:0]i,output[7:0]o); //dlaczego dwie instancje, jeśli wystarczy jedna?
    logic [3:0]a;
    logic [3:0]b;
    logic [3:0] zero = 'b0000;
    decoder_2_to_4 inst_a(a,i[1:0]); //obie robią to samo, ale chciałem zadbać o zgodność z treścią
    decoder_2_to_4 inst_b(b,i[1:0]);
    
    always_comb begin
        if(i[2])
            o = {b,zero}; //dlaczego {b,'b0000} nie syntezowało się do pożądanego układu? (poza always_comb także)
        else
            o = {zero,a};
    end
endmodule

//wersja z jedną instancją
/*
module decoder_3_to_8(input [2:0]i,output[7:0]o);
    logic [3:0]a;
    logic [3:0] zero = 'b0000;
    decoder_2_to_4 inst(a,i[1:0]); 
    
    always_comb begin
        if(i[2])
            o = {a,zero};
        else
            o = {zero,a};
    end
endmodule*/