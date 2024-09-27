with Ada.Exceptions;
with Ada.Text_IO;

package body WL.Unit is

   ------------
   -- Append --
   ------------

   procedure Append
     (To   : in out Test_Suite;
      Test : Unit_Test'Class)
   is
   begin
      To.List.Append
        (Unit_Test_Record'
           (Test   => Unit_Test_Holders.To_Holder (Test),
            Result => Test_Result_Holders.Empty_Holder));
   end Append;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests
     (Suite   : in out Test_Suite;
      Success : out Natural;
      Failure : out Natural;
      Error   : out Natural;
      Not_Run : out Natural)
   is
   begin
      Success := 0;
      Failure := 0;
      Error   := 0;
      Not_Run := 0;
      for Item of Suite.List loop
         begin
            declare
               Result : constant Test_Result :=
                          Item.Test.Element.Try;
            begin
               Item.Result := Test_Result_Holders.To_Holder (Result);
               if Is_Success (Result) then
                  Success := Success + 1;
                  if Suite.Is_Verbose then
                     Ada.Text_IO.Put_Line (Item.Test.Element.Name
                                           & ": OK");
                  end if;
               else
                  Ada.Text_IO.Put_Line (Item.Test.Element.Name
                                        & ": expected "
                                        & Expected (Result));
                  Ada.Text_IO.Put_Line (Item.Test.Element.Name
                                        & ": found "
                                        & Found (Result));
                  Failure := Failure + 1;
               end if;
            end;
         exception
            when E : others =>
               Item.Result :=
                 Test_Result_Holders.To_Holder
                   (Test_Error (Ada.Exceptions.Exception_Message (E)));
               Error := Error + 1;
               Ada.Text_IO.Put_Line (Item.Test.Element.Name
                                     & ": error: "
                                     & Error_Message (Item.Result.Element));
         end;
      end loop;
   end Run_Tests;

   -------------
   -- Verbose --
   -------------

   procedure Verbose
     (Suite   : in out Test_Suite;
      Enabled : Boolean := True)
   is
   begin
      Suite.Is_Verbose := Enabled;
   end Verbose;

end WL.Unit;
