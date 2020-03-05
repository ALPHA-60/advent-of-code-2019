with Ada.Text_Io; use Ada.Text_Io;
with Ada.Assertions; use Ada.Assertions;
package body Day_04 is

   function Is_Ok_Password_1(Number : Natural) return Boolean is
      subtype Digit is Integer range 0 .. 9;
      Last_Digit : Digit;
      N : Natural := Number;
      Has_Repeated_Digits : Boolean := False;
   begin
      loop
         Last_Digit := N mod 10;
         N := N / 10;
         exit when N = 0 or else N mod 10 > Last_Digit;
         if N mod 10 = Last_Digit then
            Has_Repeated_Digits := True;
         end if;
      end loop;
      return N = 0 and Has_Repeated_Digits;
   end Is_Ok_Password_1;

   function Is_Ok_Password_2(Number : Natural) return Boolean is
      subtype Digit is Integer range 0 .. 9;
      Last_Digit : Digit;
      N : Natural := Number;
      Duplicate_Count : Natural := 0;
      Group_Of_Exactly_Two_Appears : Boolean := False;
   begin
      loop
         Last_Digit := N mod 10;
         N := N / 10;
         if N mod 10 = Last_Digit then
            Duplicate_Count := Duplicate_Count + 1;
         else
            if Duplicate_Count = 1 then
               Group_Of_Exactly_Two_Appears := True;
            end if;
            Duplicate_Count := 0;
         end if;
         exit when N = 0 or else N mod 10 > Last_Digit;
      end loop;
      return N = 0 and Group_Of_Exactly_Two_Appears;
   end Is_Ok_Password_2;

   procedure Solve is
      Good_Password_Count : array (1..2) of Natural := (0, 0);
   begin
      for I in 273025 .. 767253 loop
         if Is_Ok_Password_1(I) then
            Good_Password_Count(1) := Good_Password_Count(1) + 1;
         end if;
         if Is_Ok_Password_2(I) then
            Good_Password_Count(2) := Good_Password_Count(2) + 1;
         end if;
      end loop;
      Put_Line("Day 04, part 1: " & Natural'Image(Good_Password_Count(1)));
      Put_Line("Day 04, part 2: " & Natural'Image(Good_Password_Count(2)));
   end Solve;
   
   
   
end Day_04;
