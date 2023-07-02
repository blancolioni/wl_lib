private with Ada.Containers.Vectors;

with WL.Binary_IO;

package WL.Images is

   type Color_Element is mod 256;

   type Image_Color is
      record
         Red   : Color_Element := 0;
         Green : Color_Element := 0;
         Blue  : Color_Element := 0;
         Alpha : Color_Element := 0;
      end record;

   for Image_Color'Size use 32;

   type Layer_Count is new Natural;
   subtype Layer_Index is Layer_Count range 1 .. Layer_Count'Last;

   type Pixel_X_Count is new Natural;
   subtype Pixel_X_Range is Pixel_X_Count range 1 .. Pixel_X_Count'Last;

   type Pixel_Y_Count is new Natural;
   subtype Pixel_Y_Range is Pixel_Y_Count range 1 .. Pixel_Y_Count'Last;

   type Image_Type is tagged private;

   function Number_Of_Layers
     (Image : Image_Type'Class)
      return Layer_Count;

   function Width
     (Image : Image_Type'Class;
      Layer : Layer_Index := Layer_Index'First)
      return Pixel_X_Count
     with Pre => Layer <= Image.Number_Of_Layers;

   function Height
     (Image : Image_Type'Class;
      Layer : Layer_Index := Layer_Index'First)
      return Pixel_Y_Count
     with Pre => Layer <= Image.Number_Of_Layers;

   function Color
     (Image : Image_Type'Class;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range)
      return Image_Color
     with Pre => X <= Image.Width and then Y <= Image.Height;

   function Color
     (Image : Image_Type'Class;
      Layer : Layer_Index;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range)
      return Image_Color
     with Pre => Layer <= Image.Number_Of_Layers
     and then X <= Image.Width (Layer)
     and then Y <= Image.Height (Layer);

   procedure Create
     (Image  : in out Image_Type'Class;
      Width  : Pixel_X_Count;
      Height : Pixel_Y_Count;
      Layers : Layer_Count := 1);

   procedure Set_Color
     (Image : in out Image_Type'Class;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range;
      Color : Image_Color)
     with Pre => X <= Image.Width
     and then Y <= Image.Height
     and then Image.Number_Of_Layers = 1;

   procedure Set_Color
     (Image : in out Image_Type'Class;
      Layer : Layer_Index;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range;
      Color : Image_Color)
     with Pre => X <= Image.Width (Layer)
     and then Y <= Image.Height (Layer)
     and then Layer <= Image.Number_Of_Layers;

   type Image_Reader is interface;

   procedure Read
     (Reader : Image_Reader;
      File   : in out WL.Binary_IO.File_Type;
      Image  : out Image_Type'Class)
   is abstract;

   procedure Read
     (Reader : Image_Reader'Class;
      Path   : String;
      Image  : out Image_Type'Class);

   type Image_Writer is interface;

   procedure Write
     (Writer : Image_Writer;
      File   : in out WL.Binary_IO.File_Type;
      Image  : Image_Type'Class)
   is abstract;

   procedure Write
     (Writer : Image_Writer'Class;
      Path   : String;
      Image  : Image_Type'Class);

private

   type Image_Data is
     array (Pixel_X_Range range <>, Pixel_Y_Range range <>) of Image_Color;

   type Image_Data_Access is access Image_Data;

   type Image_Layer_Record is
      record
         Width  : Pixel_X_Count;
         Height : Pixel_Y_Count;
         Data   : Image_Data_Access;
      end record;

   package Image_Layer_Vectors is
     new Ada.Containers.Vectors (Layer_Index, Image_Layer_Record);

   type Image_Type is tagged
      record
         Layers : Image_Layer_Vectors.Vector;
      end record;

   function Number_Of_Layers
     (Image : Image_Type'Class)
      return Layer_Count
   is (Image.Layers.Last_Index);

   function Width
     (Image : Image_Type'Class;
      Layer : Layer_Index := Layer_Index'First)
      return Pixel_X_Count
   is (Image.Layers.Element (Layer).Width);

   function Height
     (Image : Image_Type'Class;
      Layer : Layer_Index := Layer_Index'First)
      return Pixel_Y_Count
   is (Image.Layers.Element (Layer).Height);

   function Color
     (Image : Image_Type'Class;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range)
      return Image_Color
   is (Image.Color (1, X, Y));

   function Color
     (Image : Image_Type'Class;
      Layer : Layer_Index;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range)
      return Image_Color
   is (Image.Layers.Element (Layer).Data (X, Y));

end WL.Images;
