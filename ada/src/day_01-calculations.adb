package body Day_01.Calculations is

   function Fuel_Required (Mass : Integer) return Integer is
      Total_Fuel   : Integer := 0;
      Current_Fuel : Integer := Mass;
   begin
      Calculation_Loop :
      while Current_Fuel >= 0 loop
         Current_Fuel := (Current_Fuel / 3) - 2;

         exit Calculation_Loop when Current_Fuel <= 0;

         Total_Fuel := Total_Fuel + Current_Fuel;
      end loop Calculation_Loop;

      return Total_Fuel;
   end Fuel_Required;

end Day_01.Calculations;
