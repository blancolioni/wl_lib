with WL.Unit.Identity_Test;

package body WL.Json.Tests is

   function Test_Serialize_Deserialize (S : String) return String
   is (Deserialize (S).Serialize);

   ----------------
   -- Load_Tests --
   ----------------

   procedure Load_Tests
     (To : in out WL.Unit.Test_Suite)
   is
      procedure Identity
        (Name : String;
         Input : String);

      --------------
      -- Identity --
      --------------

      procedure Identity
        (Name  : String;
         Input : String)
      is
      begin
         To.Append
           (WL.Unit.Identity_Test.Test
              ("Deserialize " & Name,
               Input, Test_Serialize_Deserialize'Access));
      end Identity;

   begin
      Identity ("'null'", "null");
      Identity ("'true'", "true");
      Identity ("'false'", "false");
      Identity ("'123'", "123");
      Identity ("'123.456'", "1.23456E+02");
      Identity ("string", """hello""");
      Identity ("empty array", "[]");
      Identity ("array", "[1,2,3]");
      Identity ("empty object", "{}");
      Identity ("simple object", "{""a"":1}");
      Identity ("nested object", "{""a"":{""b"":2,""c"":null}}");
      Identity ("nested array", "{""a"":{""b"":[2,""x"",false]}}");
   end Load_Tests;

end WL.Json.Tests;
