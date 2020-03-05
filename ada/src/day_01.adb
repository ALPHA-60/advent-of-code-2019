with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.IO_Exceptions;   use Ada.IO_Exceptions;
with Day_01.Calculations; use Day_01.Calculations;

package body Day_01 is

   procedure Solve_Part_One (Input : Ada.Text_IO.File_Type) is
      Mass       : Integer;
      Total_Fuel : Integer := 0;
   begin
      while True loop
         begin
            Get (Input, Mass);
         exception
            when End_Error =>
               exit;
         end;
         Total_Fuel := Total_Fuel + (Mass / 3) - 2;
      end loop;
      Ada.Text_IO.Put_Line ("Day 01, part 1:" & Integer'Image (Total_Fuel));
   end Solve_Part_One;

   procedure Solve_Part_Two (Input : Ada.Text_IO.File_Type) is
      Mass       : Integer;
      Total_Fuel : Integer := 0;
   begin
      while True loop
         begin
            Get (Input, Mass);
         exception
            when End_Error =>
               exit;
         end;
         Total_Fuel := Total_Fuel + Fuel_Required (Mass);
      end loop;
      Ada.Text_IO.Put_Line ("Day 01, part 2:" & Integer'Image (Total_Fuel));

   end Solve_Part_Two;

end Day_01;
