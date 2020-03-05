with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Day_02 is
   package V is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);
   use V;

   procedure Interpret (Intcode : in out V.Vector) is
      Instruction_Pointer : Cursor := Intcode.First;
      Operation, Operand_1, Operand_2, Destination, Result :  Natural;
   begin
      while Element (Instruction_Pointer) /= 99 loop
         Operation   := Element(Instruction_Pointer); Next(Instruction_Pointer);
         Operand_1   := Element(Instruction_Pointer); Next(Instruction_Pointer);
         Operand_2   := Element(Instruction_Pointer); Next(Instruction_Pointer);
         Destination := Element(Instruction_Pointer); Next(Instruction_Pointer);
         Result :=
           (case Operation is
               when 1 =>
                  Intcode.Element (Operand_1) + Intcode.Element (Operand_2),
               when others =>
                  Intcode.Element (Operand_1) * Intcode.Element (Operand_2));
         Intcode.Replace_Element (Destination, Result);
      end loop;
   end Interpret;

   procedure Solve_Part_One (Input : Ada.Text_IO.File_Type) is
      Intcode : V.Vector := V.Empty_Vector;
      Byte    : Natural;
      Comma   : Character;
   begin
      Read_File :
      loop
         Get (Input, Byte);
         Intcode.Append (Byte);
         exit when End_Of_File (Input);
         Get (Input, Comma);
      end loop Read_File;
      Intcode.Replace_Element (1, 12);
      Intcode.Replace_Element (2, 2);
      Interpret (Intcode);
      Ada.Text_IO.Put_Line
        ("Day 02, Part 1:" & Natural'Image (Intcode.Element (0)));
   end Solve_Part_One;

   procedure Solve_Part_Two (Input : Ada.Text_IO.File_Type) is
      Intcode : V.Vector := V.Empty_Vector;
   begin
      Read_File :
      loop
         declare
            Byte  : Natural;
            Comma : Character;
         begin
            Get (Input, Byte);
            Intcode.Append (Byte);
            exit when End_Of_File (Input);
            Get (Input, Comma);
         end;
      end loop Read_File;

      declare
         Result : Natural;
         I      : Natural := 0;
         Code   : V.Vector;
      begin
         Search_Input :
         loop
            Code := Intcode; -- copy vector
            Code.Replace_Element (1, I / 100);
            Code.Replace_Element (2, I mod 100);
            Interpret (Code);
            Result := Code.Element (0);
            exit when Result = 19690720;
            I := I + 1;
         end loop Search_Input;

         Ada.Text_IO.Put_Line ("Day 02, Part 2:" & Natural'Image (I));
      end;

   end Solve_Part_Two;
end Day_02;
