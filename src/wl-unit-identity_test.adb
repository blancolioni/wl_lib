package body WL.Unit.Identity_Test is

   type Identity_Test_Type (Name_Length, Input_Length : Natural) is
     new Unit_Test with
      record
         Name   : String (1 .. Name_Length);
         Input  : String (1 .. Input_Length);
         Runner : Test_Runner;
      end record;

   overriding function Name
     (This : Identity_Test_Type)
      return String
   is (This.Name);

   overriding function Try
     (This : Identity_Test_Type)
      return Test_Result;

   function Test
     (Name     : String;
      Value    : String;
      Run_Test : Test_Runner)
      return Unit_Test'Class
   is (Identity_Test_Type'
         (Name_Length => Name'Length,
          Input_Length => Value'Length,
          Name         => Name,
          Input        => Value,
          Runner       => Run_Test));

   ---------
   -- Try --
   ---------

   overriding function Try
     (This : Identity_Test_Type)
      return Test_Result
   is
      Expected : constant String := This.Input;
      Found    : constant String := This.Runner (This.Input);
   begin
      if Expected = Found then
         return Test_Success;
      else
         return Test_Failure (Expected, Found);
      end if;
   end Try;

end WL.Unit.Identity_Test;
