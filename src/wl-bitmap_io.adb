with Ada.Unchecked_Deallocation;

package body WL.Bitmap_IO is

   use WL.Binary_IO;

   procedure Free is
     new Ada.Unchecked_Deallocation (Bitmap_Data,
                                     Bitmap_Data_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Bitmap_Color_Index_Data,
                                     Bitmap_Color_Index_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Colormap_Type,
                                     Colormap_Access);

   Reversed : Boolean := False;

   type Bitmap_Magic is new String (1 .. 2);
   for Bitmap_Magic'Size use 16;

   type Bitmap_Header is
      record
         File_Size   : Word_32;
         Creator_1   : Word_16;
         Creator_2   : Word_16;
         Data_Start  : Word_32;
      end record;

   for Bitmap_Header'Size use 96;

   type Bitmap_Information_Header is
      record
         Header_Size       : Word_32;
         Width             : Word_32;
         Height            : Word_32;
         Num_Planes        : Word_16;
         Bits_Per_Pixel    : Word_16;
         Compression       : Word_32;
         Image_Size        : Word_32;
         Horizontal_Res    : Word_32;
         Vertical_Res      : Word_32;
         Colormap_Size    : Word_32;
         Important_Colors : Word_32;
      end record;

   for Bitmap_Information_Header'Size use 40 * 8;

   -----------------------
   -- Adjust_Brightness --
   -----------------------

   function Adjust_Brightness
     (Color     : Color_Type;
      Factor     : Float)
      return Color_Type
   is
      R : constant Float :=
            Float'Min
              (Float (Color.R) * Factor, 255.0);
      G : constant Float :=
            Float'Min
              (Float (Color.G) * Factor, 255.0);
      B : constant Float :=
            Float'Min
              (Float (Color.B) * Factor, 255.0);
   begin
      return (R => Color_Element (R),
              G => Color_Element (G),
              B => Color_Element (B),
              Alpha => 255);
   end Adjust_Brightness;

   -----------
   -- Close --
   -----------

   procedure Close (Bitmap : in out Bitmap_Type) is
   begin
      if Bitmap.Data /= null then
         Free (Bitmap.Data);
      end if;
      if Bitmap.Indices /= null then
         Free (Bitmap.Indices);
      end if;
      if Bitmap.Colormap /= null then
         Free (Bitmap.Colormap);
      end if;
   end Close;

   ------------
   -- Color --
   ------------

   function Color (Item : Bitmap_Type;
                    X, Y : Natural)
                   return Color_Type
   is
   begin
      if Has_Colormap (Item) then
         return Colormap_Color (Item, Color_Index (Item, X, Y));
      else
         return Item.Data (X, Y);
      end if;
   end Color;

   ------------------
   -- Color_Index --
   ------------------

   function Color_Index (Item : Bitmap_Type;
                          X, Y : Natural)
                         return Color_Element
   is
   begin
      return Item.Indices (X, Y);
   end Color_Index;

   ----------------------
   -- Colormap_Color --
   ----------------------

   function Colormap_Color (Item  : Bitmap_Type;
                              Index : Color_Element)
                             return Color_Type
   is
   begin
      return Item.Colormap (Index);
   end Colormap_Color;

   -----------
   -- Depth --
   -----------

   function Depth (Item : Bitmap_Type) return Natural is
   begin
      return Item.Depth;
   end Depth;

   -------------------
   -- Has_Colormap --
   -------------------

   function Has_Colormap (Item : Bitmap_Type) return Boolean is
   begin
      return Item.Colormap /= null;
   end Has_Colormap;

   ------------
   -- Height --
   ------------

   function Height (Item : Bitmap_Type) return Natural is
   begin
      return Item.Height;
   end Height;

   --------------------------
   -- Linear_Interpolation --
   --------------------------

   function Linear_Interpolation
     (Start_Color  : Color_Type;
      Finish_Color : Color_Type;
      Start_Value   : Integer;
      Finish_Value  : Integer;
      Value         : Integer)
      return Color_Type
   is
      function Interpolate (Start, Finish : Color_Element)
                            return Color_Element;

      -----------------
      -- Interpolate --
      -----------------

      function Interpolate (Start, Finish : Color_Element)
                            return Color_Element
      is
      begin
         return Color_Element ((Integer (Finish) - Integer (Start)) *
                                (Value - Start_Value)
                                / (Finish_Value - Start_Value)
                               + Integer (Start));
      end Interpolate;

   begin
      if Start_Value = Finish_Value then
         return Start_Color;
      else
         return (Interpolate (Start_Color.B, Finish_Color.B),
                 Interpolate (Start_Color.G, Finish_Color.G),
                 Interpolate (Start_Color.R, Finish_Color.R),
                 Interpolate (Start_Color.Alpha, Finish_Color.Alpha));
      end if;
   end Linear_Interpolation;

   ----------------
   -- New_Bitmap --
   ----------------

   function New_Bitmap (Width, Height : Natural) return Bitmap_Type is
      Result : constant Bitmap_Type :=
        (Width        => Width,
         Height       => Height,
         Depth        => 32,
         Data         => new Bitmap_Data (0 .. Width - 1, 0 .. Height - 1),
         Indices      => null,
         Colormap    => null);
   begin
      Result.Data.all := (others => (others => (0, 0, 0, 1)));
      return Result;
   end New_Bitmap;

   ----------
   -- Read --
   ----------

   procedure Read (Bitmap    : out Bitmap_Type;
                   File_Name :     String)
   is
      File           : File_Type;
   begin
      Open (File, In_File, File_Name);
      Read (Bitmap, File);
      Close (File);
   exception
      when others =>
         Close (File);
         raise;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (Bitmap  : out Bitmap_Type;
                   File    : in out WL.Binary_IO.File_Type)
   is
      Magic          : Bitmap_Magic;
      Header         : Bitmap_Header;
      Info_Header    : Bitmap_Information_Header;
      Row_Size       : Word_32;
      BPP            : Word_32;
      Used           : array (Color_Element) of Natural;
   begin
      Copy (File, 0, 2, Magic'Address);
      if Magic /= "BM" then
         raise Constraint_Error with
           "bad magic number: " & String (Magic);
      end if;

      Copy (File, 2, Header'Size / 8, Header'Address);
      Copy (File, 14, Info_Header'Size / 8, Info_Header'Address);

      BPP := Word_32 (Info_Header.Bits_Per_Pixel);
      Bitmap.Depth := Natural (BPP);

      Bitmap.Width := Natural (Info_Header.Width);
      Bitmap.Height := Natural (Info_Header.Height);

      if BPP <= 8 then
         Bitmap.Indices :=
           new Bitmap_Color_Index_Data (0 .. Bitmap.Width - 1,
                                         0 .. Bitmap.Height - 1);
         Bitmap.Colormap := new Colormap_Type;
         Copy (File, 14 + Info_Header.Header_Size,
               2**Natural (BPP) * 4,
               Bitmap.Colormap.all'Address);
         Used := (others => 0);
      else

         Bitmap.Data := new Bitmap_Data (0 .. Bitmap.Width - 1,
                                         0 .. Bitmap.Height - 1);
      end if;

      Row_Size := Info_Header.Width * BPP / 8;
      if Row_Size mod 4 /= 0 then
         Row_Size := Row_Size + 4 - Row_Size mod 4;
      end if;

      for Y in 0 .. Bitmap.Height - 1 loop
         for X in 0 .. Bitmap.Width - 1 loop
            declare
               Row_Offset   : constant Word_32 :=
                                Header.Data_Start
                                  + Word_32 (Y) * Row_Size;
               Col_Offset   : constant Word_32 :=
                                Word_32 (X) * BPP / 8;
               Bit_Offset   : constant Natural :=
                                X * Natural (BPP) mod 8;
               Color       : Color_Type := (0, 0, 0, 0);
            begin
               if BPP >= 24 then
                  Copy (File, Row_Offset + Col_Offset, BPP / 8,
                        Color'Address);
                  if Reversed then
                     Bitmap.Data (X, Bitmap.Height - Y - 1) := Color;
                  else
                     Bitmap.Data (X, Y) := Color;
                  end if;
               else
                  declare
                     W8    : Word_8;
                     Index : Color_Element;
                  begin
                     Read (File, W8, Row_Offset + Col_Offset);
                     if BPP < 8 then
                        W8 := W8 / (2 ** Bit_Offset)
                        mod (2 ** Natural (BPP));
                     end if;
                     Index := Color_Element (W8);
                     Used (Index) := Used (Index) + 1;
                     if Reversed then
                        Bitmap.Indices (X, Bitmap.Height - Y - 1) := Index;
                     else
                        Bitmap.Indices (X, Y) := Index;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;

   end Read;

   ----------------
   -- Set_Color --
   ----------------

   procedure Set_Color (Item   : Bitmap_Type;
                         X, Y   : Natural;
                         Color : Color_Type)
   is
   begin
      Item.Data (X, Y) := Color;
   end Set_Color;

   -----------------------
   -- Set_Vertical_Flip --
   -----------------------

   procedure Set_Vertical_Flip (Value : Boolean := True) is
   begin
      Reversed := Value;
   end Set_Vertical_Flip;

   -----------
   -- Width --
   -----------

   function Width (Item : Bitmap_Type) return Natural is
   begin
      return Item.Width;
   end Width;

   -----------
   -- Write --
   -----------

   procedure Write (Bitmap    :    Bitmap_Type;
                    File_Name :    String)
   is
      File           : Binary_IO.File_Type;
      Magic          : constant Bitmap_Magic := "BM";
      Header         : Bitmap_Header;
      Info_Header    : Bitmap_Information_Header;
   begin
      Binary_IO.Create (File, Binary_IO.Out_File, File_Name);
      Binary_IO.Write (File, 2, Magic'Address);
      Header.File_Size := 16#36# +
        32 * Word_32 (Bitmap.Width) * Word_32 (Bitmap.Height);
      Header.Creator_1 := 16#424C#;
      Header.Creator_2 := 16#414F#;
      Header.Data_Start := 16#00000036#;
      Binary_IO.Write (File, Header'Size / 8, Header'Address);

      Info_Header.Header_Size := 16#0000_0028#;
      Info_Header.Width       := Word_32 (Bitmap.Width);
      Info_Header.Height      := Word_32 (Bitmap.Height);
      Info_Header.Num_Planes  := 1;
      Info_Header.Bits_Per_Pixel := 32;
      Info_Header.Compression    := 0;
      Info_Header.Image_Size     :=
        32 * Word_32 (Bitmap.Width) * Word_32 (Bitmap.Height);
      Info_Header.Horizontal_Res := 2835;
      Info_Header.Vertical_Res   := 2835;
      Info_Header.Colormap_Size := 0;
      Info_Header.Important_Colors := 0;

      Write (File, Info_Header'Size / 8, Info_Header'Address);

      for Y in 0 .. Bitmap.Height - 1 loop
         for X in 0 .. Bitmap.Width - 1 loop
            declare
               Color : constant Color_Type :=
                          (if Bitmap.Depth >= 24
                           then Bitmap.Data (X, Y)
                           else Bitmap.Colormap (Bitmap.Indices (X, Y)));
            begin
               Write (File, Word_8 (Color.B));
               Write (File, Word_8 (Color.G));
               Write (File, Word_8 (Color.R));
               Write (File, Word_8 (Color.Alpha));
            end;
         end loop;
      end loop;

      Close (File);

   end Write;

end WL.Bitmap_IO;
