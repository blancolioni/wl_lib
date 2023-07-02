with Ada.Strings.Fixed;

package body WL.Generic_Quantities is

   Local_Random_Unit_Real : Random_Unit_Real;

   function Significant_Digits_Image
     (Item : Real;
      Sig  : Positive)
      return String;

   Real_To_Quantity_Scale : constant Real := 10.0 ** Decimal_Places;
   Quantity_To_Real_Scale : constant Real := 1.0 / Real_To_Quantity_Scale;

   ---------
   -- "*" --
   ---------

   overriding function "*"
     (Left, Right : Quantity_Type)
      return Quantity_Type
   is
   begin
      return To_Quantity (To_Real (Left) * To_Real (Right));
   end "*";

   ---------
   -- "/" --
   ---------

   overriding function "/"
     (Left, Right : Quantity_Type)
      return Quantity_Type
   is
   begin
      return To_Quantity (To_Real (Left) / To_Real (Right));
   end "/";

   ------------
   -- Around --
   ------------

   function Around (X          : Quantity_Type;
                    Inflection : Unit_Real := 0.1;
                    Shape      : Distribution_Type := Linear)
                    return Quantity_Type
   is
      Factor : Unit_Real := 1.0;
   begin
      if Local_Random_Unit_Real = null then
         return X;
      end if;

      case Shape is
         when Linear =>
            Factor :=
              1.0 - Inflection + Local_Random_Unit_Real.all * 2.0 * Factor;
         when Quadratic =>
            Factor :=
              1.0 - Inflection + Local_Random_Unit_Real.all * 2.0 * Factor;
         when Normal =>
            Factor :=
              1.0 - Inflection + Local_Random_Unit_Real.all * 2.0 * Factor;
      end case;

      return To_Quantity (To_Real (X) * Factor);
   end Around;

   -----------
   -- Image --
   -----------

   function Image (Item : Quantity_Type) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Quantity_Type'Image (Item), Ada.Strings.Left);
   end Image;

   ---------
   -- Max --
   ---------

   function Max (Left, Right : Quantity_Type) return Quantity_Type is
   begin
      return Quantity_Type'Max (Left, Right);
   end Max;

   ---------
   -- Min --
   ---------

   function Min (Left, Right : Quantity_Type) return Quantity_Type is
   begin
      return Quantity_Type'Min (Left, Right);
   end Min;

   -----------
   -- Scale --
   -----------

   function Scale
     (X      : Quantity_Type;
      Factor : Real)
      return Quantity_Type
   is
   begin
      return To_Quantity (To_Real (X) * Factor);
   end Scale;

   ----------------
   -- Scale_Down --
   ----------------

   function Scale_Down
     (Value       : Quantity_Type;
      Numerator   : Quantity_Type;
      Denominator : Quantity_Type)
      return Quantity_Type
   is
   begin
      return Value * Numerator / Denominator;
   end Scale_Down;

   ---------------------------
   -- Set_Random_Unit_Real --
   ---------------------------

   procedure Set_Random_Unit_Real
     (Fn : Random_Unit_Real)
   is
   begin
      Local_Random_Unit_Real := Fn;
   end Set_Random_Unit_Real;

   ----------
   -- Show --
   ----------

   function Show (Item : Quantity_Type) return String is

      Factors    : constant array (1 .. 3) of Real :=
                     (1.0E9, 1.0E6, 1.0E3);
      Extensions : constant String := "GMK";
      R          : constant Real := To_Real (Item);
   begin

      if R = 0.0 then
         return "0";
      elsif R < 1.0 then
         declare
            Img : constant String :=
                    Natural'Image
                      (Natural
                         (Real_To_Quantity_Scale * R)
                       + Natural (Real_To_Quantity_Scale * 10.0));
            Last : Natural := Img'Last;
         begin
            while Last > 0 and then Img (Last) = '0' loop
               Last := Last - 1;
            end loop;
            if Last = 0 or else Img (Last) not in '0' .. '9' then
               return "0.0";
            else
               return "0." & Img (Img'First + 3 .. Last);
            end if;
         end;
      else
         for I in Factors'Range loop
            if R > Factors (I) then
               return Significant_Digits_Image (R / Factors (I), 3) &
               (1 => Extensions (I));
            end if;
         end loop;

         return Ada.Strings.Fixed.Trim
           (Natural'Image (Natural (R)), Ada.Strings.Left);
      end if;
   end Show;

   ------------------------------
   -- Significant_Digits_Image --
   ------------------------------

   function Significant_Digits_Image (Item : Real;
                                      Sig  : Positive)
                                     return String
   is
      Result    : String (1 .. Sig);
      Point     : Natural := 0;
      Acc       : Real := Item;
      Boundary  : constant Real := 10.0**Sig;
   begin
      if Item < 1.0 / Boundary then
         return "0.00";
      end if;

      if abs Item >= Boundary then
         return Ada.Strings.Fixed.Trim (Integer'Image (Integer (Item)),
                                        Ada.Strings.Left);
      else
         while abs Acc * 10.0 < Boundary loop
            Acc := Acc * 10.0;
            Point := Point + 1;
         end loop;

         Result :=
           Ada.Strings.Fixed.Trim (Integer'Image (Integer (Acc - 0.5)),
                                   Ada.Strings.Left);
         if Point < Sig then
            if Point = 0 then
               return Result;
            else
               declare
                  Before : constant String :=
                             Result (1 .. Result'Last - Point);
                  After  : constant String :=
                             Result (Result'Last - Point + 1 .. Result'Last);
               begin
                  if (for all Ch of After => Ch = '0') then
                     return Before;
                  else
                     return Before & "." & After;
                  end if;
               end;
            end if;
         else
            declare
               Zeroes : constant String (1 .. Point - Sig) :=
                 (others => '0');
            begin
               return "0." & Zeroes & Result;
            end;
         end if;
      end if;
   end Significant_Digits_Image;

   ----------------
   -- To_Natural --
   ----------------

   function To_Natural (Value : Quantity_Type) return Natural is
   begin
      return Natural (To_Real (Value));
   end To_Natural;

   -----------------
   -- To_Quantity --
   -----------------

   function To_Quantity (Value : Real) return Quantity_Type is
   begin
      return Quantity_Type (Real (Value * Real_To_Quantity_Scale));
   end To_Quantity;

   -----------------
   -- To_Quantity --
   -----------------

   function To_Quantity (Value : Natural) return Quantity_Type is
   begin
      return To_Quantity (Real (Value));
   end To_Quantity;

   --------------
   -- To_Real --
   --------------

   function To_Real (Value : Quantity_Type) return Real is
   begin
      return Real (Value) * Quantity_To_Real_Scale;
   end To_Real;

   ----------
   -- Unit --
   ----------

   function Unit return Quantity_Type is
   begin
      return To_Quantity (1.0);
   end Unit;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Quantity_Type is
      R : Real;
   begin
      if Image = "" then
         R := 0.0;
      elsif Image (Image'First) = '~' then
         return Around (Value (Image (Image'First + 1 .. Image'Last)));
      elsif Image (Image'Last) = 'K' then
         R := Real'Value (Image (Image'First .. Image'Last - 1)) * 1.0E3;
      elsif Image (Image'Last) = 'M' then
         R := Real'Value (Image (Image'First .. Image'Last - 1)) * 1.0E6;
      elsif Image (Image'Last) = 'G' then
         R := Real'Value (Image (Image'First .. Image'Last - 1)) * 1.0E9;
      else
         R := Real'Value (Image);
      end if;
      return To_Quantity (R);
   end Value;

   ----------
   -- Zero --
   ----------

   function Zero return Quantity_Type is
   begin
      return 0;
   end Zero;

end WL.Generic_Quantities;
