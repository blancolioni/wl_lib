private with System.Storage_Elements;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Sequential_IO;
private with WL.String_Maps;

package WL.Files.ELF is

   Bad_Magic : exception;

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File);

   type Program_Header_Type is
     (Nul, Load, Dynamic, Interp, Note);

   type Section_Header_Type is
     (Nul, Progbits, Symtab, Strtab, Rela, Hash, Dynamic, Note,
      Nobits, Rel, Shlib);

   type Octet is mod 2 ** 8;

   type Address_32 is mod 2 ** 32;

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Path : String);

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Path : String;
      Form : String := "");

   procedure Close
     (File : in out File_Type);

   procedure New_Section_Header
     (File : in out File_Type;
      Name : String;
      Header_Type : Section_Header_Type;
      Write       : Boolean := False;
      Alloc       : Boolean := False;
      Execinstr   : Boolean := False;
      Merge       : Boolean := False;
      Strings     : Boolean := False;
      Info_Link   : Boolean := False;
      Link_Order  : Boolean := False);

   procedure New_Program_Header
     (File            : in out File_Type;
      Header_Type     : Program_Header_Type;
      Read            : Boolean;
      Write           : Boolean;
      Execute         : Boolean;
      Virtual_Address : Address_32);

   type Symbol_Table_Binding is (Local, Global, Weak);

   type Symbol_Table_Type is
     (No_Type, Object, Func, Section, File, Common, Tls);

   type Symbol_Table_Visibility is
     (Default, Internal, Hidden, Protect);

   procedure New_Symbol
     (File         : in out File_Type;
      Name         : String;
      Value        : Address_32;
      Size         : Natural;
      Binding      : Symbol_Table_Binding;
      Typ          : Symbol_Table_Type;
      Visibility   : Symbol_Table_Visibility;
      Section_Name : String);

   procedure Add_Symbol_Reference
     (File           : in out File_Type;
      Name           : String;
      Section_Name   : String;
      Section_Offset : Address_32;
      Info           : Octet);

   procedure Put
     (File  : in out File_Type;
      Value : Octet);

private

   type Elf_Word_8 is mod 2 ** 8 with Size => 8;
   type Elf_Word_16 is mod 2 ** 16 with Size => 16;
   type Elf_Word_32 is mod 2 ** 32 with Size => 32;

   type EI_Magic is (EI_MAG0, EI_MAG1, EI_MAG2, EI_MAG3,
                     EI_CLASS, EI_DATA, EI_VERSION, EI_OSABI,
                     EI_ABIVERSION, EI_PAD1, EI_PAD2, EI_PAD3,
                     EI_PAD4, EI_PAD5, EI_PAD6, EI_PAD7);

   type Elf_Magic is array (EI_Magic) of Elf_Word_8 with Size => 16 * 8;

   type Elf_Type is new Elf_Word_16;
   type Elf_Machine is new Elf_Word_16;
   type Elf_Version is new Elf_Word_32;
   type Elf_Entry is new Elf_Word_32;
   type Elf_Phoff is new Elf_Word_32;
   type Elf_Shoff is new Elf_Word_32;
   type Elf_Flags is new Elf_Word_32;
   type Elf_Ehsize is new Elf_Word_16;
   type Elf_Phentsize is new Elf_Word_16;
   type Elf_Phnum is new Elf_Word_16;
   type Elf_Shentsize is new Elf_Word_16;
   type Elf_Shnum is new Elf_Word_16;
   type Elf_Shstrndx is new Elf_Word_16;

   type Elf_Header is
      record
         E_Magic      : Elf_Magic :=
                          (16#7F#, 16#45#, 16#4C#, 16#46#,
                           1, 1, 1,
                           others => 0);
         E_Type       : Elf_Type := 0;
         E_Machine    : Elf_Machine := 0;
         E_Version    : Elf_Version := 1;
         E_Entry      : Elf_Entry := 0;
         E_Phoff      : Elf_Phoff := 0;
         E_Shoff      : Elf_Shoff := 0;
         E_Flags      : Elf_Flags := 0;
         E_Ehsize     : Elf_Ehsize := 0;
         E_Phentsize  : Elf_Phentsize := 0;
         E_Phnum      : Elf_Phnum := 0;
         E_Shentsize  : Elf_Shentsize := 0;
         E_Shnum      : Elf_Shnum := 0;
         E_Shstrndx   : Elf_Shstrndx := 0;
      end record
   with Pack, Size => 52 * 8;

   type Section_Header is
      record
         Sh_Name      : Elf_Word_32;
         Sh_Type      : Elf_Word_32;
         Sh_Flags     : Elf_Word_32;
         Sh_Addr      : Elf_Word_32;
         Sh_Offset    : Elf_Word_32;
         Sh_Size      : Elf_Word_32;
         Sh_Link      : Elf_Word_32;
         Sh_Info      : Elf_Word_32;
         Sh_Addralign : Elf_Word_32;
         Sh_Entsize   : Elf_Word_32;
      end record
   with Pack, Size => 40 * 8;

   type Symbol_Table_Entry is
      record
         St_Name  : Elf_Word_32;
         St_Value : Address_32;
         St_Size  : Elf_Word_32;
         St_Info  : Elf_Word_8;
         St_Other : Elf_Word_8;
         St_Shndx : Elf_Word_16;
      end record
     with Pack, Size => 4 * 32;

   type Relocation_Entry is
      record
         Offset : Address_32;
         Info   : Elf_Word_32;
      end record
     with Pack, Size => 64;

   package Storage_Element_Vectors is
     new Ada.Containers.Vectors
       (Positive, System.Storage_Elements.Storage_Element,
        System.Storage_Elements."=");

   type Section_Record is
      record
         Header : Section_Header;
         Index  : Elf_Word_16;
         Data   : Storage_Element_Vectors.Vector;
      end record;

   package Section_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Section_Record);

   package Section_Maps is
     new WL.String_Maps (Section_Lists.Cursor, Section_Lists."=");

   package Storage_IO is
     new Ada.Sequential_IO (System.Storage_Elements.Storage_Element);

   package Symbol_Index_Maps is
     new WL.String_Maps (Elf_Word_32);

   type File_Type is limited
      record
         Header       : Elf_Header;
         Section_List : Section_Lists.List;
         Section_Map  : Section_Maps.Map;
         Current      : Section_Lists.Cursor;
         Symbol_Table : Section_Record;
         Symbol_Map   : Symbol_Index_Maps.Map;
         String_Table : Section_Record;
         Storage_File : Storage_IO.File_Type;
         Data         : Storage_Element_Vectors.Vector;
         Saving       : Boolean := False;
      end record;

end WL.Files.ELF;
