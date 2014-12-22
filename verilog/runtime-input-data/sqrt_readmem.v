// $ iverilog -osqrt_readmem.vvp sqrt_readmem.v sqrt.v
// $ vvp sqrt_readmem.vvp

module main;
   reg clk, reset;
   reg [31:0] data[4:0];
   reg [31:0] x;
   wire [15:0] y;
   wire        rdy;

   sqrt32 dut(clk, rdy, reset, x, y);

   always #10 clk = ~clk;

   integer     i;
   initial begin
      // Load the data set from the hex file.
      $readmemh("sqrt.hex", data);
      for (i = 0; i <= 4; i = i + 1) begin
         clk = 0;
         reset = 1;

         x = data[i];

         #35 reset = 0;

         wait (rdy) $display("y=%d", y);
      end
      $finish;
   end // initial begin
endmodule // main
