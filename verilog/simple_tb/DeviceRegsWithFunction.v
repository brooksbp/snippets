module device_regs_withfunction
  (address,
   write_en,
   data_in,
   read_en,
   read_data,
   clk,
   resetb);

   input [3:0]  address;
   input        write_en;
   input        read_en;
   input [7:0]  data_in;
   output [7:0] read_data;
   input        clk;
   input        resetb;

   reg [7:0]    reg0, reg1, reg2, reg3;
   reg [7:0]    read_data, read_data_nxt;

   function [7:0] dev_reg_nxt;
      input [3:0] address;
      input [3:0] reg_offset;
      input       write_en;
      input [7:0] data_in;
      input [7:0] dev_reg;

      begin
         dev_reg_nxt = ((address == reg_offset) && write_en) ? data_in : dev_reg;
      end
   endfunction //

   always @(posedge clk or negedge resetb)
     begin
        if (!resetb)
          begin
             reg0 <= 'd0;
             reg1 <= 'd0;
             reg2 <= 'd0;
             reg3 <= 'd0;
             read_data <= 'd0;
          end
        else
          begin
             reg0 <= dev_reg_nxt (address, 4'b0000, write_en, data_in, reg0);
             reg1 <= dev_reg_nxt (address, 4'b0001, write_en, data_in, reg1);
             reg2 <= dev_reg_nxt (address, 4'b0010, write_en, data_in, reg2);
             reg3 <= dev_reg_nxt (address, 4'b0011, write_en, data_in, reg3);
             read_data <= read_data_nxt;
          end // else: !if(!reset b)
     end

   always @(*)
     begin
        read_data_nxt = read_data;
        if (read_en) begin
           case (1'b1)
             (address == 4'b0000): read_data_nxt = reg0;
             (address == 4'b0001): read_data_nxt = reg1;
             (address == 4'b0010): read_data_nxt = reg2;
             (address == 4'b0011): read_data_nxt = reg3;
           endcase // case (1'b1)
        end
     end // always @ (*)

endmodule // device_regs_withfunction
