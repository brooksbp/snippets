// $ iverilog -o hello hello.v
// $ vvp hello

module main;
   initial
     begin
        $display("Hello, World");
        $finish;
     end
endmodule // main

