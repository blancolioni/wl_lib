with Ada.Text_IO;

with WL.Unit;
with WL.Json.Tests;

procedure Tests is
   Success, Failure, Error, Not_Run : Natural;
   Suite                            : WL.Unit.Test_Suite;
begin
   WL.Json.Tests.Load_Tests (Suite);
   Suite.Run_Tests (Success, Failure, Error, Not_Run);
   Ada.Text_IO.Put_Line ("Successes:" & Success'Image);
   Ada.Text_IO.Put_Line ("Failures: " & Failure'Image);
   Ada.Text_IO.Put_Line ("Errors:   " & Error'Image);
   Ada.Text_IO.Put_Line ("Not run:  " & Not_Run'Image);
end Tests;
