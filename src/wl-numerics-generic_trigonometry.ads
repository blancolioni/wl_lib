private with Ada.Numerics;

generic
   type Real is digits <>;
package WL.Numerics.Generic_Trigonometry is

   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   type Angle is private;

   Pi : constant Angle;

   function "+" (X, Y : Angle) return Angle;
   function "-" (X, Y : Angle) return Angle;

   function "+" (X : Angle) return Angle;
   function "-" (X : Angle) return Angle;

   function "<" (X, Y : Angle) return Boolean;
   function ">" (X, Y : Angle) return Boolean;
   function "<=" (X, Y : Angle) return Boolean;
   function ">=" (X, Y : Angle) return Boolean;

   function "abs" (X : Angle) return Angle;

   function "*" (X : Real; Y : Angle) return Angle;
   function "*" (X : Angle; Y : Real) return Angle;
   function "/" (X : Angle; Y : Real) return Angle;

   function Sin (Theta : Angle) return Signed_Unit_Real;
   function Cos (Theta : Angle) return Signed_Unit_Real;
   function Tan (Theta : Angle) return Real;

   function Arcsin (Y : Signed_Unit_Real) return Angle;
   function Arccos (Y : Signed_Unit_Real) return Angle;

   function Arctan (Y : Real;
                    X : Real := 1.0)
                    return Angle;

   function From_Radians (Radians : Real) return Angle;
   function From_Degrees (Degrees : Real) return Angle;

   function To_Radians (Theta : Angle) return Real;
   function To_Degrees (Theta : Angle) return Real;

   function Image (X : Angle) return String;
   function Value (X : String) return Angle;

private

   type Angle is new Real range -Ada.Numerics.Pi .. Ada.Numerics.Pi;

   Pi : constant Angle := Angle (Ada.Numerics.Pi);

   pragma Import (Intrinsic, "abs");

   overriding function ">" (X, Y : Angle) return Boolean is (Y < X);
   overriding function "<=" (X, Y : Angle) return Boolean is (not (X > Y));
   overriding function ">=" (X, Y : Angle) return Boolean is (not (Y > X));

   overriding function "+" (X : Angle) return Angle is (X);
   overriding function "-" (X : Angle) return Angle is
     (Angle (-Real (X)));

   function "*" (X : Real; Y : Angle) return Angle
   is (From_Radians (X * To_Radians (Y)));

   function "*" (X : Angle; Y : Real) return Angle
   is (From_Radians (To_Radians (X) * Y));

   function "/" (X : Angle; Y : Real) return Angle
   is (From_Radians (To_Radians (X) / Y));

   function Value (X : String) return Angle
   is (From_Degrees (Real'Value (X)));

end WL.Numerics.Generic_Trigonometry;
