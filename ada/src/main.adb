with Day_01;
with Day_02;
with Day_03;
with Day_04;
with Day_05;
with Day_06;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Puzzle_Input : File_Type;
begin
   --  DAY 01

   Open (Puzzle_Input, In_File, "../day_01_input.txt");
   Day_01.Solve_Part_One (Puzzle_Input);
   Close (Puzzle_Input);

   Open (Puzzle_Input, In_File, "../day_01_input.txt");
   Day_01.Solve_Part_Two (Puzzle_Input);
   Close (Puzzle_Input);

   --  DAY 02

   Open (Puzzle_Input, In_File, "../day_02_input.txt");
   Day_02.Solve_Part_One (Puzzle_Input);
   Close (Puzzle_Input);

   Open (Puzzle_Input, In_File, "../day_02_input.txt");
   Day_02.Solve_Part_Two (Puzzle_Input);
   Close (Puzzle_Input);

   --  DAY 03

   Open (Puzzle_Input, In_File, "../day_03_input.txt");
   Day_03.Solve (Puzzle_Input);
   Close (Puzzle_Input);

   --  DAY 04
   Day_04.Solve;

--   --  DAY 05
--   Open (Puzzle_Input, In_File, "../day_05_input.txt");
--   Day_05.Solve (Puzzle_Input);
--   Close (Puzzle_Input);

   --  DAY 06
   Open (Puzzle_Input, In_File, "../day_06_input.txt");
   Day_06.Solve (Puzzle_Input);
   Close (Puzzle_Input);

end Main;
