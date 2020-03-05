with Ada.Containers.Hashed_Maps;  with Ada.Containers;
with Ada.Containers.Hashed_Sets;
use Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Hash;
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Day_06 is
   procedure Solve (Input : Ada.Text_IO.File_Type) is
      subtype Planet_Name is String (1 .. 3);
      Planet1, Planet2 : String (1 .. 3);
      Dummy_Character : Character;

      package H is new Hashed_Maps
        (Key_Type => Planet_Name,
         Element_Type => Planet_Name,
         Equivalent_Keys => "=",
         Hash => Ada.Strings.Hash
        );
      use H;

      package S is new Hashed_Sets
        (Element_Type => Planet_Name,
         Equivalent_Elements => "=",
         Hash => Ada.Strings.Hash
        );
      use S;

      Center_Of : Map := Empty_Map;
      My_Planets : Set := Empty_Set;

   begin
      --  Fill Map
      while not End_Of_File (Input) loop
         Get (Input, Planet1);
         Get (Input, Dummy_Character);
         Get (Input, Planet2);
         Center_Of.Insert (Planet2, Planet1);
      end loop;
      
      ------  Solve Day 06, part 01 ------
      declare
         Orbit_Count : Natural := 0;
         Planet : Planet_Name;
      begin
         for C in Center_Of.Iterate loop
            Planet := Key (C);
            while Planet /= "COM" loop
               Planet := Center_Of(Planet);
               Orbit_Count := Orbit_Count + 1;
            end loop;
         end loop;
         Put_Line("Day 06, part 1 " & Integer'Image(Orbit_Count));
      end;

      declare
         Current_Planet : Planet_Name;
         Santas_Planet_Count : Natural := 0;
         Total : Natural := 0;
      begin
         Current_Planet := "YOU";
         while Current_Planet /= "COM" loop
            Current_Planet := Center_Of (Current_Planet);
            My_Planets.Insert (Current_Planet);
         end loop;

         Current_Planet := "SAN";

         while not My_Planets.Contains (Current_Planet) loop
            Current_Planet := Center_Of (Current_Planet);
            Santas_Planet_Count := Santas_Planet_Count + 1;
         end loop;

         Total := Santas_Planet_Count + Natural (My_Planets.Length);
         while Current_Planet /= "COM" loop
            Total := Total - 1;
            Current_Planet := Center_Of (Current_Planet);
         end loop;

         Put_Line ("Day 06, part 2 " & Integer'Image (Total - 2));
      end;

   end Solve;
end Day_06;
