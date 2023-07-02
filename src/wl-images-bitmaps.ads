package WL.Images.Bitmaps is

   type Bitmap_Image_Reader is
     new Image_Reader with private;

   overriding procedure Read
     (Reader : Bitmap_Image_Reader;
      File   : in out WL.Binary_IO.File_Type;
      Image  : out Image_Type'Class);

   type Bitmap_Image_Writer is
     new Image_Writer with private;

   overriding procedure Write
     (Writer : Bitmap_Image_Writer;
      File   : in out WL.Binary_IO.File_Type;
      Image  : Image_Type'Class);

private

   type Bitmap_Image_Reader is
     new Image_Reader with
      record
         Flip_Vertical : Boolean := True;
      end record;

   type Bitmap_Image_Writer is
     new Image_Writer with null record;

end WL.Images.Bitmaps;
