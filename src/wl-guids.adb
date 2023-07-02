with Ada.Numerics.Discrete_Random;

package body WL.Guids is

   package Element_Random is
     new Ada.Numerics.Discrete_Random (Element);

   Gen : Element_Random.Generator;

   subtype Element_String is String (1 .. 2);

   function To_Hex (E : Element) return Element_String
     with Unreferenced;

   ----------
   -- Hash --
   ----------

   function Hash (Id : Guid) return Ada.Containers.Hash_Type is
      use Ada.Containers;
      H : Hash_Type := 0;
   begin
      for E of Id loop
         H := (H * 64) xor Hash_Type (E);
      end loop;
      return H;
   end Hash;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (S : String) return Boolean is
      Index : Natural := 0;
   begin
      if S'Length /= 36 then
         return False;
      end if;

      for Ch of S loop
         Index := Index + 1;
         if Index in 9 | 14 | 19 | 24 then
            if Ch /= '-' then
               return False;
            end if;
         else
            if Ch not in '0' .. '9'
              and then Ch not in 'a' .. 'f'
              and then Ch not in 'A' .. 'F'
            then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Valid;

   --------------
   -- New_Guid --
   --------------

   function New_Guid return Guid is
   begin
      return G : Guid do
         for E of G loop
            E := Element_Random.Random (Gen);
         end loop;
         G (7) := (G (7) and 16#0F#) or 16#40#;
         G (9) := (G (9) and 16#3F#) or 16#80#;
      end return;
   end New_Guid;

   -------------
   -- To_Guid --
   -------------

   function To_Guid (S : String) return Guid is
      Last : Positive := S'First;

      function From_Hex (Ch : Character) return Element
        with Pre => Ch in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';

      --------------
      -- From_Hex --
      --------------

      function From_Hex (Ch : Character) return Element is
      begin
         if Ch in '0' .. '9' then
            return Character'Pos (Ch) - Character'Pos ('0');
         elsif Ch in 'a' .. 'f' then
            return Character'Pos (Ch) - Character'Pos ('a') + 10;
         elsif Ch in 'A' .. 'F' then
            return Character'Pos (Ch) - Character'Pos ('A') + 10;
         else
            raise Constraint_Error with "From_Hex: precondition failed";
         end if;
      end From_Hex;

   begin
      return G : Guid do
         for E of G loop
            if S (Last) = '-' then
               Last := Last + 1;
            end if;
            E := From_Hex (S (Last)) * 16 + From_Hex (S (Last + 1));
            Last := Last + 2;
         end loop;
      end return;
   end To_Guid;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (E : Element) return Element_String is
      Ds : constant String := "0123456789abcdef";
      It : Element := E;
   begin
      return H : Element_String do
         for Ch of reverse H loop
            Ch := Ds (Natural (It mod 16) + 1);
            It := It / 16;
         end loop;
      end return;
   end To_Hex;

   ---------------
   -- To_String --
   ---------------

   function To_String (Id : Guid) return String is
      Ds     : constant String := "0123456789abcdef";

      function Hi (E : Element) return Character
      is (Ds (Natural (E / 16) + 1));

      function Lo (E : Element) return Character
      is (Ds (Natural (E mod 16) + 1));

      Index  : constant array (Id'Range) of Positive :=
                 (1, 3, 5, 7, 10, 12, 15, 17, 20, 22, 25, 27, 29, 31, 33, 35);
      Hyphens : constant array (1 .. 4) of Positive :=
                  (9, 14, 19, 24);
   begin
      return Result : String (1 .. 36) do
         for I in Id'Range loop
            Result (Index (I)) := Hi (Id (I));
            Result (Index (I) + 1) := Lo (Id (I));
         end loop;
         for H of Hyphens loop
            Result (H) := '-';
         end loop;
      end return;
   end To_String;

end WL.Guids;
