generic
   type Result_Type (<>) is private;
   with function Image (Result : Result_Type) return String;
   with function Compare (Left, Right : Result_Type) return Natural;
package WL.Unit.Compare_Test is

   type Test_Runner is access function return Result_Type;

   function Test
     (Name     : String;
      Run_Test : Test_Runner;
      Expected : Result_Type)
      return Unit_Test'Class;

end WL.Unit.Compare_Test;
