package body WL.Numerics.Roman is

   -----------------
   -- Roman_Image --
   -----------------

   function Roman_Image (Value : Positive) return String is
      subtype Digit_Range is Natural range 0 .. 9;

      function Digit_Image
        (Digit : Digit_Range;
         Unit  : Character;
         Five  : Character;
         Ten   : Character)
         return String;

      -----------------
      -- Digit_Image --
      -----------------

      function Digit_Image
        (Digit : Digit_Range;
         Unit  : Character;
         Five  : Character;
         Ten   : Character)
         return String
      is
      begin
         case Digit is
            when 0 =>
               return "";
            when 1 .. 3 =>
               return S : constant String (1 .. Digit) := (others => Unit) do
                  null;
               end return;
            when 4 =>
               return (Unit, Five);
            when 5 =>
               return (1 => Five);
            when 6 .. 8 =>
               return S : String (1 .. Digit - 4) := (others => Unit) do
                  S (1) := Five;
               end return;
            when 9 =>
               return (Unit, Ten);
         end case;
      end Digit_Image;

   begin
      return Digit_Image (Value / 100 mod 10, 'C', 'D', 'M')
        & Digit_Image (Value / 10 mod 10, 'X', 'L', 'C')
        & Digit_Image (Value mod 10, 'I', 'V', 'X');
   end Roman_Image;

end WL.Numerics.Roman;
