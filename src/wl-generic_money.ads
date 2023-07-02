with WL.Generic_Quantities;

generic
   with package Quantities is new WL.Generic_Quantities (<>);
package WL.Generic_Money is

   type Money_Type is private;

   subtype Real is Quantities.Real;

   function Zero return Money_Type;
   function Infinite return Money_Type;

   function "+" (Left : Money_Type) return Money_Type;
   function "-" (Left : Money_Type) return Money_Type;

   function "+" (Left, Right : Money_Type) return Money_Type;
   function "-" (Left, Right : Money_Type) return Money_Type;

   function "<"  (Left, Right : Money_Type) return Boolean;
   function ">"  (Left, Right : Money_Type) return Boolean;
   function "<=" (Left, Right : Money_Type) return Boolean;
   function ">=" (Left, Right : Money_Type) return Boolean;

   function "abs" (Left : Money_Type) return Money_Type;

   function Adjust (Money    : Money_Type;
                    Factor   : Quantities.Real)
                   return Money_Type;

   function To_Money (Amount : Real) return Money_Type;
   function To_Real (Amount : Money_Type) return Quantities.Real;

   function Max (X, Y : Money_Type) return Money_Type;
   function Min (X, Y : Money_Type) return Money_Type;

   function Tax (Money : Money_Type;
                 Tax   : Quantities.Real)
                 return Money_Type;

   function Without_Tax (Money : Money_Type;
                         Tax   : Real)
                         return Money_Type;

   function Add_Tax (Money    : Money_Type;
                     Tax_Rate : Real)
                     return Money_Type;

   type Price_Type is private;

   function "+" (Left, Right : Price_Type) return Price_Type;
   function "-" (Left, Right : Price_Type) return Price_Type;

   function "<"  (Left, Right : Price_Type) return Boolean;
   function ">"  (Left, Right : Price_Type) return Boolean;
   function "<=" (Left, Right : Price_Type) return Boolean;
   function ">=" (Left, Right : Price_Type) return Boolean;

   function To_Price (Amount : Real) return Price_Type;
   function To_Real (Price : Price_Type) return Real;

   function Adjust_Price (Price    : Price_Type;
                          Factor   : Real)
                         return Price_Type;

   function Tax (Price   : Price_Type;
                 Tax     : Real)
                 return Price_Type;

   function Without_Tax (Price   : Price_Type;
                         Tax     : Real)
                         return Price_Type;

   function Add_Tax (Price : Price_Type;
      Tax_Rate : Real)
                     return Price_Type;

   function Max (X, Y : Price_Type) return Price_Type;
   function Min (X, Y : Price_Type) return Price_Type;

   function Total (Price    : Price_Type;
                   Quantity_Type : Quantities.Quantity_Type)
                  return Money_Type;

   function Total (Price    : Price_Type;
                   Quantity : Real)
                   return Money_Type;

   function Price (Total    : Money_Type;
                   Quantity_Type : Quantities.Quantity_Type)
                   return Price_Type;

   function Get_Quantity
     (Total_Cash : Money_Type;
      Price      : Price_Type)
      return Quantities.Quantity_Type;

   function Image (Item : Money_Type) return String;
   function Image (Item : Price_Type) return String;
   --  Raw image functions

   function Show
     (Item  : Money_Type;
      Exact : Boolean := False)
      return String;

   function Show
     (Price : Price_Type;
      Exact : Boolean := False)
      return String;
   --  User-friendly image functions.  If Exact is True, the amount
   --  will be shown in full (down to the cent).  Otherwise, it will
   --  be abbreviatd to thousands, millions, billions or trillions.
   --  e.g. $12T instead of $12,345,678,901.23

   function Value (Image : String) return Money_Type;
   function Value (Image : String) return Price_Type;

   function Split (Amount  : Money_Type;
                   Portion : Real)
                  return Money_Type;

   function Zero return Price_Type;

   function Currency_Symbol return String;
   function Digit_Grouping_Symbol return String;
   function Decimal_Symbol return String;

   procedure Set_Image_Properties
     (Currency_Symbol       : String := "$";
      Digit_Grouping_Symbol : String := ",";
      Decimal_Symbol        : String := ".");

private

   use type Quantities.Real;

   type Money_Type is range -2 ** 63 .. 2 ** 63 - 1;
   type Price_Type is range 0 .. 2 ** 63 - 1;

   function To_Money (Amount : Real) return Money_Type
   is (Money_Type (Real'Truncation (Amount * 1000.0)));

   function To_Real (Amount : Money_Type) return Real
   is (Real (Amount) / 1000.0);

   function To_Price (Amount : Real) return Price_Type
   is (Price_Type (To_Money (Amount)));

   function To_Real (Price : Price_Type) return Real
   is (To_Real (Money_Type (Price)));

   function Total (Price    : Price_Type;
                   Quantity : Real)
                   return Money_Type
   is (To_Money (To_Real (Price) * Quantity));

   function Infinite return Money_Type
   is (Money_Type'Last);

   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

end WL.Generic_Money;
