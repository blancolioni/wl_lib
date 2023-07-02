with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body WL.Numerics.Generic_Trigonometry is

   package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Real);

   ---------
   -- "+" --
   ---------

   overriding function "+" (X, Y : Angle) return Angle is
   begin
      return From_Radians (Real (X) + Real (Y));
   end "+";

   ---------
   -- "-" --
   ---------

   overriding function "-" (X, Y : Angle) return Angle is
   begin
      return From_Radians (Real (X) - Real (Y));
   end "-";

   ---------
   -- "<" --
   ---------

   overriding function "<" (X, Y : Angle) return Boolean is
   begin
      return abs (Real (X)) < abs (Real (Y));
   end "<";

   ------------
   -- Arccos --
   ------------

   function Arccos (Y : Signed_Unit_Real) return Angle is
   begin
      return From_Radians (Elementary_Functions.Arccos (Y));
   end Arccos;

   ------------
   -- Arcsin --
   ------------

   function Arcsin (Y : Signed_Unit_Real) return Angle is
   begin
      return From_Radians (Elementary_Functions.Arcsin (Y));
   end Arcsin;

   ------------
   -- Arctan --
   ------------

   function Arctan (Y : Real;
                    X : Real := 1.0)
                    return Angle
   is
   begin
      return From_Radians (Elementary_Functions.Arctan (Y, X));
   end Arctan;

   ---------
   -- Cos --
   ---------

   function Cos (Theta : Angle) return Signed_Unit_Real is
   begin
      return Elementary_Functions.Cos (Real (Theta));
   end Cos;

   ------------------
   -- From_Degrees --
   ------------------

   function From_Degrees (Degrees : Real) return Angle is
   begin
      return From_Radians (Degrees * Ada.Numerics.Pi / 180.0);
   end From_Degrees;

   ------------------
   -- From_Radians --
   ------------------

   function From_Radians (Radians : Real) return Angle is
      R : Real := Radians;
   begin
      while R > Ada.Numerics.Pi loop
         R := R - 2.0 * Ada.Numerics.Pi;
      end loop;
      while R < -Ada.Numerics.Pi loop
         R := R + 2.0 * Ada.Numerics.Pi;
      end loop;
      return Angle (R);
   end From_Radians;

   -----------
   -- Image --
   -----------

   function Image (X : Angle) return String is
      package Length_IO is new Ada.Text_IO.Float_IO (Real);
      D : constant Real := To_Degrees (X);
      S : String (1 .. 20) := (others => ' ');
   begin
      Length_IO.Put (S, D, 10, 0);
      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Image;

   ---------
   -- Sin --
   ---------

   function Sin (Theta : Angle) return Signed_Unit_Real is
   begin
      return Elementary_Functions.Sin (Real (Theta));
   end Sin;

   ---------
   -- Tan --
   ---------

   function Tan (Theta : Angle) return Real is
   begin
      return Elementary_Functions.Tan (Real (Theta));
   end Tan;

   ----------------
   -- To_Degrees --
   ----------------

   function To_Degrees (Theta : Angle) return Real is
   begin
      return To_Radians (Theta) * 180.0 / Ada.Numerics.Pi;
   end To_Degrees;

   ----------------
   -- To_Radians --
   ----------------

   function To_Radians (Theta : Angle) return Real is
   begin
      return Real (Theta);
   end To_Radians;

end WL.Numerics.Generic_Trigonometry;
