with Ada.Characters.Handling;
with Ada.Directories;

with WL.Images.DDS;
with WL.Images.TGA;

package body WL.Images.Reader is

   ---------------------
   -- Read_Image_File --
   ---------------------

   procedure Read_Image_File
     (Image : out Image_Type'Class;
      Path  : String)
   is
      Ext : constant String :=
              Ada.Characters.Handling.To_Lower
                (Ada.Directories.Extension (Path));
   begin
      if Ext = "dds" then
         WL.Images.DDS.Read_DDS (Image, Path);
      elsif Ext = "tga" then
         WL.Images.TGA.Read_TGA (Image, Path);
      else
         raise Constraint_Error with
           "unknown image format: " & Ext;
      end if;
   end Read_Image_File;

end WL.Images.Reader;
