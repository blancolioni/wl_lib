with WL.Binary_IO;                     use WL.Binary_IO;

package body WL.Images.TGA is

   --------------
   -- Read_TGA --
   --------------

   procedure Read_TGA
     (Image : out Image_Type'Class;
      Path  : String)
   is
      pragma Unreferenced (Image);
      File              : File_Type;
      ID_Length         : Word_8;
      Color_Map_Type   : Word_8;
      Image_Type        : Word_8;
      Color_Map_Offset : Word_16;
      Color_Map_Length : Word_16;
      Color_Map_Bits   : Word_8;
      X_Origin          : Word_16;
      Y_Origin          : Word_16;
      Image_Width       : Word_16;
      Image_Height      : Word_16;
      Bits_Per_Pixel    : Word_8;
      Descriptor        : Word_8;
   begin
      Open (File, In_File, Path);

      Read (File, ID_Length);
      Read (File, Color_Map_Type);
      Read (File, Image_Type);
      Read (File, Color_Map_Offset);
      Read (File, Color_Map_Length);
      Read (File, Color_Map_Bits);
      Read (File, X_Origin);
      Read (File, Y_Origin);
      Read (File, Image_Width);
      Read (File, Image_Height);
      Read (File, Bits_Per_Pixel);
      Read (File, Descriptor);

      Close (File);

   end Read_TGA;

end WL.Images.TGA;
