private with Ada.Streams;
private with Ada.Strings.Unbounded;

with System;

package WL.Binary_IO is

   type File_Type is private;

   type File_Mode is (In_File, Out_File);

   procedure Open (File : in out File_Type;
                   Mode :        File_Mode;
                   Name :        String);

   procedure Create (File : in out File_Type;
                     Mode :        File_Mode;
                     Name :        String);

   function View
     (File : File_Type)
      return File_Type;

   procedure Close (File : in out File_Type);

   type Word_32 is mod 2 ** 32 with Size => 32;
   type Word_16 is mod 2 ** 16 with Size => 16;
   type Word_8 is mod 2 ** 8 with Size => 8;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1 with Size => 32;
   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1 with Size => 16;
   type Integer_8 is range -2 ** 7 .. 2 ** 7 - 1 with Size => 8;

   function View
     (File   : File_Type;
      Start  : Word_32;
      Length : Word_32)
      return File_Type;

   function Length (File : File_Type) return Word_32;

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_32);

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_16);

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_8);

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_32);

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_16);

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_8);

   procedure Read (File   : in out File_Type;
                   Item   :    out String);

   function Read (File       : in out File_Type;
                  Terminator : Character := Character'Val (0))
                  return String;

   procedure Read (File        : in out File_Type;
                   Size        :        Word_32;
                   Destination :        System.Address);

   procedure Read (File   : File_Type;
                   Item   : out Word_32;
                   Offset : Word_32);

   procedure Read (File   : File_Type;
                   Item   : out Word_16;
                   Offset : Word_32);

   procedure Read (File   : File_Type;
                   Item   : out Word_8;
                   Offset : Word_32);

   function Read (File   : File_Type;
                  Offset : Word_32;
                  Terminator : Character := Character'Val (0))
                  return String;

   procedure Write (File   : in out File_Type;
                    Item   :        Word_32);

   procedure Write (File   : in out File_Type;
                    Item   :        Word_16);

   procedure Write (File   : in out File_Type;
                    Item   :        Word_8);

   procedure Copy (File        : File_Type;
                   Offset      : Word_32;
                   Length      : Word_32;
                   Destination : System.Address);

   procedure Write (File    : in out File_Type;
                    Length  : Word_32;
                    Source  : System.Address);

   procedure Write (File : in out File_Type;
                    Text : String);

   function Current_Offset (File : File_Type) return Word_32;

   procedure Set_Offset (File  : in out File_Type;
                         Offset : Word_32);

   procedure Skip
     (File : in out File_Type;
      Offset : Word_32);

   function Hex_Image (Value : Word_32) return String;
   function Hex_Image (Value : Word_16) return String;
   function Hex_Image (Value : Word_8) return String;

   function End_Of_File (File : File_Type) return Boolean;

private

   type Stream_Array_Access is
     access all Ada.Streams.Stream_Element_Array;

   type File_Type is
      record
         Path   : Ada.Strings.Unbounded.Unbounded_String;
         Data   : Stream_Array_Access;
         Mode   : File_Mode;
         Size   : Ada.Streams.Stream_Element_Count;
         Offset : Word_32 := 0;
         Start  : Word_32 := 0;
      end record;

end WL.Binary_IO;
