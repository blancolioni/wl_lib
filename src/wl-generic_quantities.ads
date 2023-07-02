generic
   type Real is digits <>;
   Decimal_Places : Natural := 0;
   Max_Value : Real := Real'Last;
package WL.Generic_Quantities is

   subtype Unit_Real is Real range 0.0 .. 1.0;

   type Quantity_Type is private;

   function Zero return Quantity_Type;
   function Unit return Quantity_Type;

   function To_Quantity (Value : Real) return Quantity_Type
     with Pre => Value <= Max_Value;

   function To_Quantity (Value : Natural) return Quantity_Type;

   function To_Real (Value : Quantity_Type) return Real
     with Post => To_Real'Result <= Max_Value;

   function To_Natural (Value : Quantity_Type) return Natural;

   function Image (Item : Quantity_Type) return String;
   function Value (Image : String) return Quantity_Type;

   function Show (Item : Quantity_Type) return String;

   function "*" (Left, Right : Quantity_Type) return Quantity_Type;
   function "/" (Left, Right : Quantity_Type) return Quantity_Type;
   function "mod" (Left, Right : Quantity_Type) return Quantity_Type;
   function "+" (Left, Right : Quantity_Type) return Quantity_Type;
   function "-" (Left, Right : Quantity_Type) return Quantity_Type;
   function "<" (Left, Right : Quantity_Type) return Boolean;
   function ">" (Left, Right : Quantity_Type) return Boolean;
   function "<=" (Left, Right : Quantity_Type) return Boolean;
   function ">=" (Left, Right : Quantity_Type) return Boolean;

   function Min (Left, Right : Quantity_Type) return Quantity_Type;
   function Max (Left, Right : Quantity_Type) return Quantity_Type;

   function "abs" (X : Quantity_Type) return Quantity_Type;

   function Scale
     (X : Quantity_Type;
      Factor : Real)
      return Quantity_Type;

   function Scale_Down
     (Value       : Quantity_Type;
      Numerator   : Quantity_Type;
      Denominator : Quantity_Type)
      return Quantity_Type;

   type Random_Unit_Real is access
     function return Unit_Real;

   procedure Set_Random_Unit_Real
     (Fn : Random_Unit_Real);

   type Distribution_Type is (Linear, Quadratic, Normal);

   function Around (X          : Quantity_Type;
                    Inflection : Unit_Real := 0.1;
                    Shape      : Distribution_Type := Linear)
                    return Quantity_Type;

private

   type Quantity_Type is range 0 .. 2 ** 63 - 1;

   pragma Import (Intrinsic, "mod");
   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

end WL.Generic_Quantities;
