--  A quick'n'dirty interface to Ada.Numerics.Discrete_Random,
--  so that you don't have to instantiate it yourself.

package WL.Random is
   pragma Elaborate_Body;

   function Random_Number (Max : Natural) return Natural;
   function Random_Number (Min, Max : Integer) return Integer;

   procedure Randomise;
   procedure Reset (Initiator : Integer);

   function Current_State return String;
   procedure Restore_State (State : String);

end WL.Random;
