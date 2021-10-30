
//Robione z pomocą przykładu z wykładu
module microwave(
    input clk, nrst, door, start, finish,
    output heat, light, bell,
);
    const logic [2:0] CLOSED = 3'b000, COOK = 3'b001,
                      PAUSE  = 3'b010, OPEN = 3'b011,
                      BELL   = 3'b100;

    logic [2:0] state;

    //funkcja przejścia
    always_ff @(posedge clk or negedge nrst)
    if(!nrst) state <= CLOSED;
    else unique case(state)
        CLOSED: begin
            if(door) state <= OPEN;
            else if (start) state <= COOK;
        end
        COOK: begin
            if(door) state <= PAUSE;
            else if (finish) state <= BELL;
        end
        PAUSE: if(!door) state <= COOK;
        BELL: if(door) state <= OPEN;
        OPEN: if(!door) state <= CLOSED;
    endcase

    always_comb begin
        heat = 0;
        light = 0;
        bell = 0;

        unique case (state)
            COOK: begin heat = 1; light = 1; end
            PAUSE: begin light = 1; end
            BELL: begin bell = 1; end
            OPEN: begin light = 1; end
        endcase
    end


endmodule