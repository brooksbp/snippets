// $ iverilog -osqrt_plusargs.vvp sqrt_plusargs.v sqrt.v
// $ vvp sqrt_plusargs.vvp +x=81

module main;
   reg clk, reset;
   reg [31:0] x;
   wire [15:0] y;
   wire        rdy;

   sqrt32 dut(clk, rdy, reset, x, y);

   always #10 clk = ~clk;

   initial begin
      clk = 0;
      reset = 1;

      if (! $value$plusargs("x=%d", x)) begin
         $display("ERROR: please specify +x=<value> to start.");
         $finish;
      end

      #35 reset = 0;

      wait (rdy) $display("y=%d", y);
      $finish;

   end // initial begin
endmodule // main
