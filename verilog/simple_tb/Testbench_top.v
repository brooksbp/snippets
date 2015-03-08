`timescale 1ns/10ps

module testbench_top();

   reg [3:0] address_tb;
   reg [7:0] wrdata_tb;
   reg       write_en_tb, read_en_tb;
   reg       clk_tb, resetb;
   wire [7:0] rddata_tb;

   parameter CLKTB_HALF_PERIOD = 5;  // 100MHz clock
   parameter RST_DEASSERT_DLY = 100;

   parameter REG0_OFFSET = 4'b0000,
     REG1_OFFSET = 4'b0001,
     REG2_OFFSET = 4'b0010,
     REG3_OFFSET = 4'b0011;

   initial
     begin
        $dumpfile("out.vcd");
        $dumpvars(0,testbench_top);
     end

   // Generate clk_tb
   initial begin
      clk_tb = 1'b0;
      forever begin
         #CLKTB_HALF_PERIOD clk_tb = ~clk_tb;  // 100MHz
      end
   end

   // Generate resetb
   initial begin
      resetb = 1'b0;

      #RST_DEASSERT_DLY resetb = 1'b1;
   end

   // Initialize variables
   initial begin
      address_tb = 'h0;
      wrdata_tb = 'h0;
      write_en_tb = 1'b0;
      read_en_tb = 1'b0;
   end

   // DUT instantiation
   device_regs_withfunction device_regs_withfunction_0
     (.clk (clk_tb),
      .resetb (resetb),
      .address (address_tb),
      .write_en (write_en_tb),
      .read_en (read_en_tb),
      .data_in (wrdata_tb),
      .read_data (rddata_tb));

   task reg_write;
      input [3:0] address_in;
      input [7:0] data_in;

      begin
         @(posedge clk_tb);
         #1 address_tb = address_in;
         @(posedge clk_tb);
         #1 write_en_tb = 1'b1;
         wrdata_tb = data_in;
         @(posedge clk_tb);
         #1;
         write_en_tb = 1'b0;
         address_tb = 4'hF;
         wrdata_tb = 4'h0;
      end
   endtask //

   task reg_read;
      input [3:0] address_in;
      input [7:0] expected_data;

      begin
         @(posedge clk_tb);
         #1 address_tb = address_in;
         @(posedge clk_tb);
         #1 read_en_tb = 1'b1;
         @(posedge clk_tb);
         #1 read_en_tb = 1'b0;
         address_tb = 4'hF;
         @(posedge clk_tb);
         // use triple equal in verification so that all cases
         // (0, 1, X, and Z) are covered in comparison
         if (expected_data === rddata_tb)
           $display("data matches: expected_data = %h, actual data = %h", expected_data, rddata_tb);
         else
           $display("ERROR: data mismatch: expected_data = %h, actual data = %h", expected_data, rddata_tb);
      end
   endtask //

   initial
     begin
        #1000;
        reg_write (REG0_OFFSET, 8'hA5);
        reg_read (REG0_OFFSET, 8'hA5);
        reg_write (REG1_OFFSET, 8'hA6);
        reg_read (REG1_OFFSET, 8'hA6);
        reg_write (REG2_OFFSET, 8'hA7);
        reg_read (REG2_OFFSET, 8'hA7);
        reg_write (REG3_OFFSET, 8'hA8);
        reg_read (REG3_OFFSET, 8'hA8);
        reg_read (REG0_OFFSET, 8'hA5);
        $finish;
     end

endmodule // testbench_top
