with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Day_03 is

   type Direction_Type is (Up, Down, Left, Right);

   type Step is record
      Direction : Direction_Type;
      Distance  : Positive;
   end record;

   type Location is record
      X : Integer;
      Y : Integer;
   end record;

   function "<" (Here, There : Location) return Boolean is
   begin
      if Here.X = There.X then
         return Here.Y < There.Y;
      else
         return Here.X < There.X;
      end if;
   end "<";

   package SV is new Ada.Containers.Vectors (Index_Type => Natural,
      Element_Type                                      => Step);

   subtype Step_Vector is SV.Vector;

   function Read_Steps (Input : Ada.Text_IO.File_Type) return Step_Vector is
      Steps                        : Step_Vector;
      Current_Direction, Separator : Character;
      Distance                     : Positive;
      Eol                          : Boolean;
   begin
      loop
         Get (Input, Current_Direction);
         Get (Input, Distance);
         Steps.Append
           ((Direction =>
               (case Current_Direction is when 'U' => Up,
                  when 'R'                         => Right,
                  when 'D'                         => Down,
                  when 'L'                         => Left,
                  when others                      => raise Data_Error),
             Distance => Distance));

         Look_Ahead (Input, Separator, Eol);
         exit when Eol;
         Get (Input, Separator);
      end loop;
      return Steps;
   end Read_Steps;

   package W is new Ada.Containers.Ordered_Maps
     (Key_Type     => Location,
      Element_Type => Natural);
   subtype Wire_Type is W.Map;
   use W;

   function Steps_To_Wire (Steps : Step_Vector) return Wire_Type is
      Wire        : Wire_Type;
      X, Y        : Integer := 0;
      Wire_Length : Natural := 0;
      DX          : Integer range -1 .. 1;
      DY          : Integer range -1 .. 1;
   begin
      for Step of Steps loop
         case Step.Direction is
            when Up =>
               DX := 0;
               DY := 1;
            when Right =>
               DX := 1;
               DY := 0;
            when Down =>
               DX := 0;
               DY := -1;
            when Left =>
               DX := -1;
               DY := 0;
         end case;

         Add_Wire_Locations :
         for N in 1 .. Step.Distance loop
            X           := X + DX;
            Y           := Y + DY;
            Wire_Length := Wire_Length + 1;
            if not Contains (Wire, (X, Y)) then
               Insert (Wire, (X, Y), Wire_Length);
            end if;
         end loop Add_Wire_Locations;

      end loop;
      return Wire;
   end Steps_To_Wire;

   function Distance_To_Closest_Intersection
     (Wire_1, Wire_2 : Wire_Type) return Natural
   is
      Min_Distance  : Natural := Natural'Last;
      Wire_2_Cursor : W.Cursor;
      Grid_Place    : Location;
   begin
      for Wire_1_Cursor in Wire_1.Iterate loop
         Grid_Place    := Key (Wire_1_Cursor);
         Wire_2_Cursor := Find (Wire_2, Grid_Place);
         if Wire_2_Cursor /= No_Element then
            Min_Distance :=
              Integer'Min (Min_Distance, abs Grid_Place.X + abs Grid_Place.Y);
         end if;
      end loop;
      return Min_Distance;
   end Distance_To_Closest_Intersection;

   procedure Solve (Input : Ada.Text_IO.File_Type) is
      Wire_1        : constant Wire_Type := Steps_To_Wire (Read_Steps (Input));
      Wire_2        : constant Wire_Type := Steps_To_Wire (Read_Steps (Input));
      Best_Distance : constant Natural   :=
        Distance_To_Closest_Intersection (Wire_1, Wire_2);
   begin
      Put_Line ("Day 03, part 1:" & Natural'Image (Best_Distance));
   --      begin
      --         for Wire_1_Cursor in Wires (1).Iterate loop
      --            Grid_Place    := Key (Wire_1_Cursor);
      --            Wire_2_Cursor := Find (Wires (2), Grid_Place);
      --            if Wire_2_Cursor /= No_Element then
      --               Min_Distance :=
      --                 Integer'Min (Min_Distance,
      --                    abs Grid_Place.X + abs Grid_Place.Y);
      --               Min_Length :=
      --                 Positive'Min (Min_Length,
      --                    Element (Wire_1_Cursor) + Element (Wire_2_Cursor));
      --            end if;
      --         end loop;
      --
      --         Put_Line ("Day 03, part 1:" & Natural'Image (Min_Distance));
      --         Put_Line ("Day 03, part 2:" & Natural'Image (Min_Length));
      --      end;
   end Solve;

end Day_03;
