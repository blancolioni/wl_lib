package WL.Unit.Identity_Test is

   type Test_Runner is access function (S : String) return String;

   function Test
     (Name     : String;
      Value    : String;
      Run_Test : Test_Runner)
      return Unit_Test'Class;

end WL.Unit.Identity_Test;
