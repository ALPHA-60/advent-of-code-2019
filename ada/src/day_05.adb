with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Day_05 is
   package V is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);

   subtype Intcode_Array is V.Vector;

   procedure Execute (Intcode : in out Intcode_Array);

   type Operation_Type is
     (ADD,           MULTIPLY,  INPUT,
      OUTPUT,        HALT,      JUMP_IF_TRUE,
      JUMP_IF_FALSE, LESS_THAN, EQUALS);

   Instruction_Length : constant array (Operation_Type) of Natural  :=
     (ADD             => 4,
      MULTIPLY        => 4,
      INPUT           => 2,
      OUTPUT          => 2,
      JUMP_IF_TRUE    => 0,
      JUMP_IF_FALSE   => 0,
      LESS_THAN       => 4,
      EQUALS          => 4,
      HALT            => 0);

   function Number_To_Operation (Number : Natural) return Operation_Type is
     (case Number mod 100 is
         when 1      => ADD,
         when 2      => MULTIPLY,
         when 3      => INPUT,
         when 4      => OUTPUT,
         when 5      => JUMP_IF_TRUE,
         when 6      => JUMP_IF_FALSE,
         when 7      => LESS_THAN,
         when 8      => EQUALS,
         when 99     => HALT,
         when others => raise Constraint_Error);

   procedure Execute (Intcode : in out Intcode_Array) is
      Ip : Integer := 0;

      function Get_Operand (Offset : Natural) return Integer;
      function Get_Destination (Offset : Natural) return Integer;

      function Get_Operand (Offset : Natural) return Integer is
         Value_At_Offset : constant Integer := Intcode (Ip + Offset);
      begin
         if ((Intcode (Ip) / (10 * (10 ** Offset))) mod 10) = 0 then
            return  Intcode (Value_At_Offset);
         else
            return Value_At_Offset;
         end if;
      end Get_Operand;

      function Get_Destination (Offset : Natural) return Integer is
        (Intcode (Ip + Offset));

   begin
      loop
         declare
            Operation : constant Operation_Type := Number_To_Operation (Intcode (Ip));
         begin
            case Operation is

               when ADD =>
                  Intcode (Get_Destination (3)) :=
                    Get_Operand (1) + Get_Operand (2);

               when MULTIPLY =>
                  Intcode (Get_Destination (Offset => 3)) :=
                    Get_Operand (1) * Get_Operand (2);

               when INPUT =>
                  declare
                     Value : Integer;
                  begin
                     Get (Value);
                     Intcode (Get_Destination (1)) := Value;
                  end;

               when OUTPUT =>
                  Put_Line (Integer'Image (Get_Operand (Offset => 1)));

               when JUMP_IF_FALSE | JUMP_IF_TRUE =>
                  if (Operation = JUMP_IF_TRUE and Get_Operand (1) /= 0)
                    or (Operation = JUMP_IF_FALSE and Get_Operand (1) = 0)
                  then
                     Ip := Get_Operand (2);
                  else
                     Ip := Ip + 3;
                  end if;

               when LESS_THAN =>
                  Intcode (Get_Destination (3)) :=
                    (if Get_Operand (1) < Get_Operand (2) then 1 else 0);

               when EQUALS =>
                  Intcode (Get_Destination (3)) :=
                    (if Get_Operand (1) = Get_Operand (2) then 1 else 0);

               when HALT =>
                  exit;

            end case;
            if Operation not in JUMP_IF_TRUE | JUMP_IF_FALSE then
               Ip := Ip + Instruction_Length (Operation);
            end if;
         end;
      end loop;
   end Execute;

   procedure Solve (Input : Ada.Text_IO.File_Type) is
      Intcode : Intcode_Array := V.Empty_Vector;
      Byte    : Integer;
      Comma   : Character;
   begin
      loop
         Get (Input, Byte);
         Intcode.Append (Byte);
         exit when End_Of_File (Input);
         Get (Input, Comma);
      end loop;
      Execute (Intcode);
   end Solve;

end Day_05;
