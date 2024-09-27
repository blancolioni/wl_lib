with Ada.Containers.Indefinite_Holders;

package body WL.Unit.Compare_Test is

   package Result_Holders is
     new Ada.Containers.Indefinite_Holders (Result_Type, "=");

   type Compare_Test_Type (Name_Length : Natural) is new Unit_Test with
      record
         Name     : String (1 .. Name_Length);
         Test     : Test_Runner;
         Expected : Result_Holders.Holder;
      end record;

   overriding function Name
     (This : Compare_Test_Type)
      return String
   is (This.Name);

   overriding function Try
     (This : Compare_Test_Type)
      return Test_Result;

   ----------
   -- Test --
   ----------

   function Test
     (Name     : String;
      Run_Test : Test_Runner;
      Expected : Result_Type)
      return Unit_Test'Class
   is
   begin
      return Compare_Test_Type'
        (Name_Length => Name'Length, Name => Name,
         Test        => Run_Test,
         Expected    => Result_Holders.To_Holder (Expected));
   end Test;

   ---------
   -- Try --
   ---------

   overriding function Try
     (This : Compare_Test_Type)
      return Test_Result
   is
      Expected : constant Result_Type := This.Expected.Element;
      Result   : constant Result_Type := This.Test.all;
      Index    : constant Natural := Compare (Expected, Result);
   begin
      if Index = 0 then
         return Test_Success;
      else
         return Test_Failure
           (Image (Expected), Image (Result),
            "Results differ at position" & Index'Image);
      end if;
   end Try;

end WL.Unit.Compare_Test;
