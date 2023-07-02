package body WL.Guids.Tests is

   ----------------
   -- Test_Guids --
   ----------------

   procedure Test_Guids (Count : Positive) is
   begin
      for I in 1 .. Count loop
         declare
            G : constant Guid := New_Guid;
            S : constant String := To_String (G);
            V : constant Boolean := Is_Valid (S);
         begin
            if not V then
               raise Constraint_Error
                 with "generated invalid string " & S;
            end if;

            declare
               H : constant Guid := To_Guid (S);
            begin
               if G /= H then
                  raise Constraint_Error with
                    "guids do not match: "
                    & S & " /= " & To_String (H);
               end if;
            end;
         end;
      end loop;
   end Test_Guids;

end WL.Guids.Tests;
