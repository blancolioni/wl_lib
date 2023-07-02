with WL.Binary_IO;

package WL.Bitmap_IO is

   type Bitmap_Type is private;

   type Color_Element is mod 256;

   type Color_Type is
      record
         B, G, R : Color_Element;
         Alpha   : Color_Element;
      end record;

   for Color_Type'Size use 32;

   function Linear_Interpolation
     (Start_Color  : Color_Type;
      Finish_Color : Color_Type;
      Start_Value   : Integer;
      Finish_Value  : Integer;
      Value         : Integer)
      return Color_Type;

   function Adjust_Brightness
     (Color     : Color_Type;
      Factor     : Float)
      return Color_Type;

   procedure Read (Bitmap    : out Bitmap_Type;
                   File_Name : String);

   procedure Read (Bitmap  : out Bitmap_Type;
                   File    : in out WL.Binary_IO.File_Type);

   procedure Write (Bitmap    : Bitmap_Type;
                    File_Name : String);

   procedure Close (Bitmap : in out Bitmap_Type);

   function New_Bitmap (Width, Height : Natural) return Bitmap_Type;

   function Width (Item : Bitmap_Type) return Natural;
   function Height (Item : Bitmap_Type) return Natural;
   function Depth (Item : Bitmap_Type) return Natural;
   function Color (Item : Bitmap_Type;
                    X, Y : Natural)
                   return Color_Type;

   procedure Set_Color (Item   : Bitmap_Type;
                         X, Y   : Natural;
                         Color : Color_Type);

   function Has_Colormap (Item : Bitmap_Type) return Boolean;
   function Color_Index (Item : Bitmap_Type;
                          X, Y : Natural)
                         return Color_Element;
   function Colormap_Color (Item  : Bitmap_Type;
                              Index : Color_Element)
                             return Color_Type;

   procedure Set_Vertical_Flip (Value : Boolean := True);

private

   type Bitmap_Data is
     array (Natural range <>, Natural range <>) of Color_Type;

   pragma Pack (Bitmap_Data);

   type Bitmap_Color_Index_Data is
     array (Natural range <>, Natural range <>) of Color_Element;

   type Bitmap_Data_Access is access Bitmap_Data;
   type Bitmap_Color_Index_Access is access Bitmap_Color_Index_Data;

   type Colormap_Type is array (Color_Element) of Color_Type;

   type Colormap_Access is access Colormap_Type;

   type Bitmap_Type is
      record
         Width, Height : Natural;
         Depth         : Natural;
         Data          : Bitmap_Data_Access;
         Indices       : Bitmap_Color_Index_Access;
         Colormap     : Colormap_Access;
      end record;

end WL.Bitmap_IO;
