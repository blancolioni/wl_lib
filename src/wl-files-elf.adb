with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body WL.Files.ELF is

   Trace_Reader : constant Boolean := False;

   String_Table_Name : constant String := ".shstrtab";

   procedure Scan_Form (Text    : String;
                        Process : not null access
                          procedure (Name, Value : String));

   function Find_Section
     (File : File_Type;
      Name : String)
      return Elf_Word_16;

   function Save_Name
     (File : in out File_Type;
      Name : String)
      return Elf_Word_32;

   procedure Align
     (File      : in out File_Type;
      Alignment : System.Storage_Elements.Storage_Count);

   --  procedure Read
   --    (File  : in out File_Type;
   --     Value : out System.Storage_Elements.Storage_Element);

   procedure Read_Record
     (File   : in out File_Type;
      Offset : Elf_Word_32;
      Addr   : System.Address;
      Length : System.Storage_Elements.Storage_Count);

   procedure Read_Vector
     (File   : in out File_Type;
      Offset : Elf_Word_32;
      Count  : Natural;
      Vector : in out Storage_Element_Vectors.Vector);

   procedure Write
     (File  : in out File_Type;
      Value : System.Storage_Elements.Storage_Element);

   procedure Write
     (File   : in out File_Type;
      Addr   : System.Address;
      Length : System.Storage_Elements.Storage_Count);

   procedure Write
     (File   : in out File_Type;
      Vector : Storage_Element_Vectors.Vector);

   procedure Write_Name
     (Vector : in out Storage_Element_Vectors.Vector;
      Nm     : String);

   procedure Put_Hex
     (Value : Elf_Word_32;
      Width : Positive);

   --------------------------
   -- Add_Symbol_Reference --
   --------------------------

   procedure Add_Symbol_Reference
     (File           : in out File_Type;
      Name           : String;
      Section_Name   : String;
      Section_Offset : Address_32;
      Info           : Octet)
   is
      Rel_Name : constant String := ".rel" & Section_Name;
   begin
      if not File.Section_Map.Contains (Rel_Name) then
         File.Section_List.Append
           (Section_Record'
              (Header => Section_Header'
                   (Sh_Name      => Save_Name (File, Rel_Name),
                    Sh_Type      => Section_Header_Type'Pos (Sht_Rel),
                    Sh_Flags     => 2 ** 6,
                    Sh_Addr      => 0,
                    Sh_Offset    => 0,
                    Sh_Size      => 0,
                    Sh_Link      => 0,
                    Sh_Info      => 1,
                    Sh_Addralign => 4,
                    Sh_Entsize   => 8),
               Index  => Elf_Word_16 (File.Section_List.Length),
               Data   => <>));
         File.Section_Map.Insert (Rel_Name, File.Section_List.Last);
      end if;

      if not File.Symbol_Map.Contains (Name) then
         New_Symbol (File, Name, 0, 0, Global, Object, Default, Section_Name);
      end if;

      declare
         use System.Storage_Elements;
         Rel     : Relocation_Entry :=
                     Relocation_Entry'
                       (Offset => Section_Offset,
                        Info   => (File.Symbol_Map (Name) - 1) * 256
                        + Elf_Word_32 (Info));
         Data    : Storage_Array (1 .. Rel'Size / 8);
         pragma Import (Ada, Data);
         for Data'Address use Rel'Address;
         Section : Section_Record renames
                     File.Section_List (File.Section_Map (Rel_Name));
      begin
         for Element of Data loop
            Section.Data.Append (Element);
         end loop;
      end;
   end Add_Symbol_Reference;

   -----------
   -- Align --
   -----------

   procedure Align
     (File      : in out File_Type;
      Alignment : System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;
      Extra : constant Storage_Count :=
                Storage_Count (File.Data.Length) mod Alignment;
   begin
      if Extra > 0 then
         declare
            Pad_Size : constant Storage_Count := Alignment - Extra;
            Padding  : constant Storage_Array (1 .. Pad_Size) :=
                         (others => 0);
         begin
            Write (File, Padding'Address, Pad_Size);
         end;
      end if;
   end Align;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
      use type System.Storage_Elements.Storage_Count;
      Start_Offset : constant := 16#0040#;
   begin

      if File.Reading then
         return;
      end if;

      for Section of File.Section_List loop
         if Section.Header.Sh_Type = Section_Header_Type'Pos (Sht_Rel) then
            Section.Header.Sh_Link := Elf_Word_32 (File.Section_List.Length);
         end if;
      end loop;

      File.Symbol_Table.Header.Sh_Name :=
        Elf_Word_32 (File.String_Table.Data.Length);
      Write_Name (File.String_Table.Data, ".symtab");
      File.Symbol_Table.Header.Sh_Link :=
        Elf_Word_32 (File.Section_List.Length) + 1;
      --  File.Symbol_Table.Header.Sh_Info := 1;
      File.Section_List.Append (File.Symbol_Table);
      File.Section_Map.Insert
        (".symtab", File.Section_List.Last);

      File.String_Table.Header.Sh_Name :=
        Elf_Word_32 (File.String_Table.Data.Length);
      Write_Name (File.String_Table.Data, String_Table_Name);
      File.Section_List.Append (File.String_Table);
      File.Section_Map.Insert
        (String_Table_Name, File.Section_List.Last);

      File.Header.E_Shstrndx :=
        Elf_Shstrndx (File.Section_List.Length) - 1;

      for Section of File.Section_List loop
         Align (File, Start_Offset);
         Section.Header.Sh_Offset :=
           Elf_Word_32 (File.Data.Length) + Start_Offset;
         Section.Header.Sh_Size  := Elf_Word_32 (Section.Data.Length);
         Write (File, Section.Data);
      end loop;

      File.Header.E_Shentsize := Elf_Shentsize (Section_Header'Size / 8);
      File.Header.E_Shoff := Elf_Shoff (File.Data.Length) + Start_Offset;
      File.Header.E_Shnum := Elf_Shnum (File.Section_List.Length);
      for Section of File.Section_List loop
         Write (File, Section.Header'Address, Section.Header'Size / 8);
      end loop;

      File.Saving := True;

      Write (File, File.Header'Address, File.Header'Size / 8);
      for I in 1 .. Start_Offset - File.Header'Size / 8 loop
         Write (File, 0);
      end loop;

      Write (File, File.Data);

      Storage_IO.Close (File.Storage_File);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Path : String;
      Form : String := "")
   is
      pragma Unreferenced (Mode);

      procedure Set_Form (Name, Value : String);

      --------------
      -- Set_Form --
      --------------

      procedure Set_Form (Name, Value : String) is
      begin
         if Name = "endien" then
            if Value = "big" then
               File.Header.E_Magic (EI_DATA) := 2;
            end if;
         elsif Name = "word-size" then
            if Value = "64" then
               File.Header.E_Magic (EI_CLASS) := 2;
            end if;
         end if;
      end Set_Form;

   begin
      Scan_Form (Form, Set_Form'Access);
      Storage_IO.Create (File.Storage_File, Storage_IO.Out_File, Path);
      File.Header.E_Type := 16#01#;
      File.Header.E_Machine := 16#F3#;
      File.Header.E_Ehsize := File.Header'Size / 8;
      File.Symbol_Table :=
        Section_Record'
          (Header => Section_Header'
             (Sh_Name      => 0,
              Sh_Type      => Section_Header_Type'Pos (Sht_Symtab),
              Sh_Flags     => 0,
              Sh_Addr      => 0,
              Sh_Offset    => 0,
              Sh_Size      => 16,
              Sh_Link      => 0,
              Sh_Info      => 1,
              Sh_Addralign => 0,
              Sh_Entsize   => 16),
           Index  => 0,
           Data   => <>);
      for I in 1 .. File.Symbol_Table.Header.Sh_Size loop
         File.Symbol_Table.Data.Append (0);
      end loop;
      File.String_Table :=
        Section_Record'
          (Header => Section_Header'
             (Sh_Name      => 0,
              Sh_Type      => Section_Header_Type'Pos (Sht_Strtab),
              Sh_Flags     => 0,
              Sh_Addr      => 0,
              Sh_Offset    => 0,
              Sh_Size      => 0,
              Sh_Link      => 0,
              Sh_Info      => 0,
              Sh_Addralign => 0,
              Sh_Entsize   => 0),
           Index  => 0,
           Data   => <>);
      for I in 1 .. 4 loop
         File.String_Table.Data.Append (0);
      end loop;

      File.Section_List.Append
        (Section_Record'
           (Header => Section_Header'
                (Sh_Name      => 0,
                 Sh_Type      => 0,
                 Sh_Flags     => 0,
                 Sh_Addr      => 0,
                 Sh_Offset    => 0,
                 Sh_Size      => 0,
                 Sh_Link      => 0,
                 Sh_Info      => 0,
                 Sh_Addralign => 0,
                 Sh_Entsize   => 0),
            Index  => 0,
            Data   => <>));
   end Create;

   ------------------
   -- Find_Section --
   ------------------

   function Find_Section
     (File : File_Type;
      Name : String)
      return Elf_Word_16
   is
   begin
      return File.Section_List (File.Section_Map (Name)).Index;
   end Find_Section;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (File   : File_Type;
      Symbol : Symbol_Table_Entry)
      return String
   is
   begin
      return Get_String (File, Symbol.St_Name);
   end Get_Name;

   -----------------
   -- Get_Section --
   -----------------

   function Get_Section
     (File           : File_Type;
      Section_Header : Section_Header_Type)
      return Elf_Word_32
   is
      Index : Elf_Word_32 := 0;
   begin
      for Section of File.Section_List loop
         if Section.Header.Sh_Type
           = Section_Header_Type'Pos (Section_Header)
         then
            return Index;
         end if;
         Index := Index + 1;
      end loop;
      return 0;
   end Get_Section;

   -----------------------
   -- Get_Section_Entry --
   -----------------------

   function Get_Section_Entry
     (File           : File_Type;
      Section_Header : Section_Header_Type)
      return Section_Entry
   is
   begin
      for Position in File.Section_List.Iterate loop
         if Section_Lists.Element (Position).Header.Sh_Type
           = Section_Header_Type'Pos (Section_Header)
         then
            return (Position => Position);
         end if;
      end loop;
      return (Position => Section_Lists.No_Element);
   end Get_Section_Entry;

   -----------------------
   -- Get_Section_Entry --
   -----------------------

   function Get_Section_Entry
     (File  : File_Type;
      Index : Elf_Word_32)
      return Section_Entry
   is
   begin
      for Position in File.Section_List.Iterate loop
         if Elf_Word_32 (Section_Lists.Element (Position).Index) = Index then
            return (Position => Position);
         end if;
      end loop;
      return (Position => Section_Lists.No_Element);
   end Get_Section_Entry;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (File   : File_Type;
      Offset : Elf_Word_32)
      return String
   is
      use type System.Storage_Elements.Storage_Element;
      Str_Sec : constant Section_Entry :=
                  Get_Section_Entry (File, Sht_Strtab);
      Rec     : Section_Record renames
                  File.Section_List (Str_Sec.Position);
      First   : constant Natural := Natural (Offset);
      Last    : Natural := First;
   begin
      while Last <= Rec.Data.Last_Index
        and then Rec.Data (Last) /= 0
      loop
         Last := Last + 1;
      end loop;
      return S : String (1 .. Last - First) do
         for I in S'Range loop
            S (I) := Character'Val (Rec.Data (First + I - 1));
         end loop;
      end return;
   end Get_String;

   ------------------------
   -- Iterate_Relocation --
   ------------------------

   procedure Iterate_Relocation
     (File    : File_Type;
      Process : not null access
        procedure (Section      : Elf_Word_16;
                   Offset       : Address_32;
                   Info         : Octet;
                   Symbol       : Symbol_Table_Entry))
   is
      Size    : constant Positive :=
                  Natural (Relocation_Entry'Size) / 8;
   begin
      for Rec of File.Section_List loop
         if Section_Header_Type'Val (Rec.Header.Sh_Type) = Sht_Rel then
            declare
               Symbols : constant Elf_Word_16 :=
                           Elf_Word_16 (Rec.Header.Sh_Link);
               Section : constant Elf_Word_16 :=
                           Elf_Word_16 (Rec.Header.Sh_Info);
               Offset  : Natural := 0;
            begin
               while Offset <= Rec.Data.Last_Index - Size + 1 loop
                  declare
                     use System.Storage_Elements;
                     Data : Storage_Array (1 .. Storage_Offset (Size));
                     Ent  : Relocation_Entry;
                     for Ent'Address use Data'Address;
                  begin
                     for Element of Data loop
                        Element := Rec.Data.Element (Offset);
                        Offset := Offset + 1;
                     end loop;

                     declare
                        --  Sym_Idx : constant Elf_Word_32 := Ent.Info / 256;
                        Sym_Ent : constant Symbol_Table_Entry :=
                                    Get_Symbol_Entry
                                      (File, Elf_Word_32 (Symbols),
                                       Ent.Info / 256);
                     begin
                        Process (Section, Ent.Offset,
                                 Octet (Ent.Info mod 8),
                                 Sym_Ent);
                     end;
                  end;
               end loop;
            end;
         end if;
      end loop;
   end Iterate_Relocation;

   ----------------------
   -- Iterate_Sections --
   ----------------------

   procedure Iterate_Sections
     (File        : File_Type;
      Header_Type : Section_Header_Type;
      Process     : not null access
        procedure (Section : Section_Entry))
   is
   begin
      for Position in File.Section_List.Iterate loop
         if File.Section_List (Position).Header.Sh_Type
           = Section_Header_Type'Pos (Header_Type)
         then
            Process ((Position => Position));
         end if;
      end loop;
   end Iterate_Sections;

   ---------------------
   -- Iterate_Symbols --
   ---------------------

   procedure Iterate_Symbols
     (File    : File_Type;
      Process : not null access
        procedure (Name         : String;
                   Value        : Address_32;
                   Size         : Elf_Word_32;
                   Binding      : Symbol_Table_Binding;
                   Typ          : Symbol_Table_Type;
                   Visibility   : Symbol_Table_Visibility;
                   Section      : Elf_Word_16))
   is
      Rec : Section_Record renames
              File.Section_List
                (Get_Section_Entry (File, Sht_Symtab).Position);
      Size   : constant Positive := Natural (Symbol_Table_Entry'Size) / 8;
      Offset : Natural := Size;
   begin
      while Offset <= Rec.Data.Last_Index - Size + 1 loop
         declare
            use System.Storage_Elements;
            Data : Storage_Array (1 .. Storage_Offset (Size));
            Ent  : Symbol_Table_Entry;
            for Ent'Address use Data'Address;
         begin
            for Element of Data loop
               Element := Rec.Data.Element (Offset);
               Offset := Offset + 1;
            end loop;
            Process (Get_String (File, Ent.St_Name),
                     Ent.St_Value,
                     Ent.St_Size,
                     Symbol_Table_Binding'Val (Ent.St_Info / 16),
                     Symbol_Table_Type'Val (Ent.St_Info mod 16),
                     Default,
                     Ent.St_Shndx);
         end;
      end loop;
   end Iterate_Symbols;

   ------------------------
   -- New_Program_Header --
   ------------------------

   procedure New_Program_Header
     (File            : in out File_Type;
      Header_Type     : Program_Header_Type;
      Read            : Boolean;
      Write           : Boolean;
      Execute         : Boolean;
      Virtual_Address : Address_32)
   is
   begin
      null;
   end New_Program_Header;

   ------------------------
   -- New_Section_Header --
   ------------------------

   procedure New_Section_Header
     (File        : in out File_Type;
      Name        : String;
      Header_Type : Section_Header_Type;
      Write       : Boolean := False;
      Alloc       : Boolean := False;
      Execinstr   : Boolean := False;
      Merge       : Boolean := False;
      Strings     : Boolean := False;
      Info_Link   : Boolean := False;
      Link_Order  : Boolean := False)
   is
      function Flag (B     : Boolean;
                     Shift : Natural)
                     return Elf_Word_32
      is (if B then 2 ** Shift else 0);

      Section_Flags : constant Elf_Word_32 :=
                        Flag (Write, 0)
                        + Flag (Alloc, 1)
                        + Flag (Execinstr, 2)
                        + Flag (Merge, 4)
                        + Flag (Strings, 5)
                        + Flag (Info_Link, 6)
                        + Flag (Link_Order, 7);

      Rec : constant Section_Record :=
              (Header => Section_Header'
                 (Sh_Name      => Save_Name (File, Name),
                  Sh_Type      => Section_Header_Type'Pos (Header_Type),
                  Sh_Flags     => Section_Flags,
                  Sh_Addr      => 0,
                  Sh_Offset    => 0,
                  Sh_Size      => 0,
                  Sh_Link      => 0,
                  Sh_Info      => 0,
                  Sh_Addralign => 0,
                  Sh_Entsize   => 0),
               Index  => Elf_Word_16 (File.Section_List.Length),
               Data   => Storage_Element_Vectors.Empty_Vector);
   begin
      File.Section_List.Append (Rec);
      File.Section_Map.Insert (Name, File.Section_List.Last);
      File.Current := File.Section_List.Last;
   end New_Section_Header;

   ----------------
   -- New_Symbol --
   ----------------

   procedure New_Symbol
     (File         : in out File_Type;
      Name         : String;
      Value        : Address_32;
      Size         : Natural;
      Binding      : Symbol_Table_Binding;
      Typ          : Symbol_Table_Type;
      Visibility   : Symbol_Table_Visibility;
      Section_Name : String)
   is
      use System.Storage_Elements;
      Shndx : constant Elf_Word_16 :=
                (if Section_Name = "" then 0
                 else Find_Section (File, Section_Name));
      Sym   : constant Symbol_Table_Entry :=
                Symbol_Table_Entry'
                  (St_Name    => Save_Name (File, Name),
                   St_Value   => Value,
                   St_Size    => Elf_Word_32 (Size),
                   St_Info    => Symbol_Table_Binding'Pos (Binding) * 2 ** 4
                   + Symbol_Table_Type'Pos (Typ) mod 16,
                   St_Other   => Symbol_Table_Visibility'Pos (Visibility),
                   St_Shndx   => Shndx);
      Data  : Storage_Array (1 .. Sym'Size / 8);
      pragma Import (Ada, Data);
      for Data'Address use Sym'Address;
   begin
      for Element of Data loop
         File.Symbol_Table.Data.Append (Element);
      end loop;

      File.Symbol_Map.Insert
        (Name, Elf_Word_32 (File.Symbol_Table.Data.Length)
         / Elf_Word_32 (Data'Length));

      if Binding = Local then
         declare
            Sh_Info : Elf_Word_32 renames
                        File.Symbol_Table.Header.Sh_Info;
         begin
            Sh_Info := Sh_Info + 1;
         end;
      end if;

   end New_Symbol;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Path : String)
   is
      use type System.Storage_Elements.Storage_Offset;
   begin
      File.Reading := True;

      Storage_IO.Open (File.Storage_File, Storage_IO.In_File, Path);
      File.Data.Clear;
      while not Storage_IO.End_Of_File (File.Storage_File) loop
         declare
            X : System.Storage_Elements.Storage_Element;
         begin
            Storage_IO.Read (File.Storage_File, X);
            File.Data.Append (X);
         end;
      end loop;
      Storage_IO.Close (File.Storage_File);

      Read_Record (File, 0, File.Header'Address, File.Header'Size / 8);

      if Trace_Reader then
         Ada.Text_IO.Put ("  Magic:   ");
         for X of File.Header.E_Magic loop
            declare
               S : constant String := "0123456789abcdef";
               I : constant Positive := Natural (X / 16) + 1;
               J : constant Positive := Natural (X mod 16) + 1;
            begin
               Ada.Text_IO.Put (' ');
               Ada.Text_IO.Put (S (I));
               Ada.Text_IO.Put (S (J));
            end;
         end loop;
         Ada.Text_IO.New_Line;
      end if;

      if File.Header.E_Magic /= Elf_Magic_Sequence then
         Ada.Text_IO.Put
           ("magic: [");
         for Ch of File.Header.E_Magic loop
            if Ch in 32 .. 126 then
               Ada.Text_IO.Put (Character'Val (Ch));
            else
               Ada.Text_IO.Put
                 ("#"
                  & Character'Val (48 + Ch / 100)
                  & Character'Val (48 + Ch / 10 mod 10)
                  & Character'Val (48 + Ch mod 10));
            end if;
         end loop;
         Ada.Text_IO.Put_Line ("]");
         raise Bad_Magic;
      end if;

      if Trace_Reader then
         Ada.Text_IO.Put_Line
           ("  Class:                             ");
         Ada.Text_IO.Put_Line
           ("  Start of section headers:         "
            & File.Header.E_Shoff'Image
            & " (bytes into the file)");
         Ada.Text_IO.Put_Line
           ("  Size of section headers:          "
            & File.Header.E_Shentsize'Image
            & " (bytes)");
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put_Line
           ("Section Headers:");
         Ada.Text_IO.Put_Line
           ("  [Nr] Name              Type            Addr     Off    Size"
            & "   ES Flg Lk Inf Al");
      end if;

      declare
         Sh_Base : Elf_Word_32 := Elf_Word_32 (File.Header.E_Shoff);
         Sh_Idx  : Elf_Shnum := 0;
      begin
         while Sh_Idx < File.Header.E_Shnum loop
            declare
               Rec : Section_Record;
            begin
               Read_Record (File, Sh_Base,
                            Rec.Header'Address, Rec.Header'Size / 8);
               Rec.Index := Elf_Word_16 (Sh_Idx);
               if Rec.Header.Sh_Size > 0 then
                  Read_Vector
                    (File, Rec.Header.Sh_Offset,
                     Natural (Rec.Header.Sh_Size), Rec.Data);
               end if;
               File.Section_List.Append (Rec);

               Sh_Idx := Sh_Idx + 1;
               Sh_Base := Sh_Base + Elf_Word_32 (File.Header.E_Shentsize);
            end;
         end loop;

         if Trace_Reader then
            for Section of File.Section_List loop
               declare
                  use Ada.Text_IO, Ada.Integer_Text_IO;

                  procedure Put_String
                    (Text  : String;
                     Width : Positive);

                  ----------------
                  -- Put_String --
                  ----------------

                  procedure Put_String
                    (Text  : String;
                     Width : Positive)
                  is
                     S : String (1 .. Width) := (others => ' ');
                     Last : constant Natural :=
                              Natural'Min (Text'Length, Width);
                  begin
                     S (1 .. Last) :=
                       Text (Text'First .. Text'First + Last - 1);
                     Put (S);
                  end Put_String;

               begin
                  Put ("  [");
                  Put (Natural (Section.Index), 2);
                  Put ("] ");
                  Put_String (Get_String (File, Section.Header.Sh_Name), 18);
                  declare
                     Sh_Type : Section_Header_Type;
                  begin
                     Sh_Type :=
                       Section_Header_Type'Val (Section.Header.Sh_Type);

                     declare
                        Img : constant String :=
                                Section_Header_Type'Image (Sh_Type);
                     begin
                        Put_String (Img (5 .. Img'Last), 16);
                     end;
                  exception
                     when Constraint_Error =>
                        Put_String ("UNKNOWN", 16);
                  end;

                  Put_Hex (Section.Header.Sh_Addr, 8);
                  Put (" ");
                  Put_Hex (Section.Header.Sh_Offset, 6);
                  Put (" ");
                  Put_Hex (Section.Header.Sh_Size, 6);

               end;

               Ada.Text_IO.New_Line;
            end loop;
         end if;

      end;

   end Open;

   ---------
   -- Put --
   ---------

   procedure Put (File : in out File_Type; Value : Octet) is
   begin
      File.Section_List (File.Section_List.Last)
        .Data.Append (System.Storage_Elements.Storage_Element (Value));
   end Put;

   -------------
   -- Put_Hex --
   -------------

   procedure Put_Hex
     (Value : Elf_Word_32;
      Width : Positive)
   is
      S  : constant String := "0123456789abcdef";
      It : Elf_Word_32 := Value;
      R  : String (1 .. Width);
   begin
      for Ch of reverse R loop
         Ch := S (Natural (It mod 16) + 1);
         It := It / 16;
      end loop;
      Ada.Text_IO.Put (R);
   end Put_Hex;

   ----------
   -- Read --
   ----------

   procedure Read
     (File    : in out File_Type;
      Offset  : Elf_Word_32;
      Data    : out System.Storage_Elements.Storage_Array)
   is
      Index : Natural := Natural (Offset);
      Dump  : constant Boolean := False;
   begin
      for Element of Data loop
         declare
            Dx : constant Natural := Index - Natural (Offset);
         begin
            if Dump then
               if Dx mod 16 = 0 then
                  Put_Hex (Elf_Word_32 (Dx), 4);
                  Ada.Text_IO.Put (":");
               end if;
            end if;

            if Index <= File.Data.Last_Index then
               Element := File.Data (Index);
            else
               Element := 0;
            end if;

            if Dump then
               Ada.Text_IO.Put (" ");
               Put_Hex (Elf_Word_32 (Element), 2);
            end if;

         end;

         Index := Index + 1;

         if Dump then
            if (Index - Natural (Offset)) mod 16 = 0 then
               Ada.Text_IO.New_Line;
            end if;
         end if;

      end loop;

      if Dump then
         if (Index - Natural (Offset)) mod 16 /= 0 then
            Ada.Text_IO.New_Line;
         end if;
      end if;

   end Read;

   -----------------
   -- Read_Record --
   -----------------

   procedure Read_Record
     (File   : in out File_Type;
      Offset : Elf_Word_32;
      Addr   : System.Address;
      Length : System.Storage_Elements.Storage_Count)
   is
      Data : System.Storage_Elements.Storage_Array (1 .. Length);
      for Data'Address use Addr;
   begin
      Read (File, Offset, Data);
   end Read_Record;

   -----------------
   -- Read_Vector --
   -----------------

   procedure Read_Vector
     (File   : in out File_Type;
      Offset : Elf_Word_32;
      Count  : Natural;
      Vector : in out Storage_Element_Vectors.Vector)
   is
      Data : System.Storage_Elements.Storage_Array
        (1 .. System.Storage_Elements.Storage_Offset (Count));
   begin
      Read (File, Offset, Data);
      Vector.Clear;
      for Element of Data loop
         Vector.Append (Element);
      end loop;
   end Read_Vector;

   ---------------
   -- Save_Name --
   ---------------

   function Save_Name
     (File : in out File_Type;
      Name : String)
   return Elf_Word_32
   is
   begin
      return Addr : constant Elf_Word_32 :=
        Elf_Word_32 (File.String_Table.Data.Length)
      do
         Write_Name (File.String_Table.Data, Name);
      end return;
   end Save_Name;

   ---------------
   -- Scan_Form --
   ---------------

   procedure Scan_Form (Text    : String;
                        Process : not null access
                          procedure (Name, Value : String))
   is
      Source : constant String := Text & ";";
      Start  : Positive := Source'First;
      Index  : Natural := Ada.Strings.Fixed.Index (Source, ";");
   begin
      while Index > 0 loop
         declare
            Name_And_Value : constant String := Source (Start .. Index - 1);
            Colon_Index    : constant Natural :=
                               Ada.Strings.Fixed.Index (Name_And_Value, ":");
         begin
            if Colon_Index > 0 then
               declare
                  Name  : constant String :=
                            Name_And_Value
                              (Name_And_Value'First .. Colon_Index - 1);
                  Value : constant String :=
                            Name_And_Value
                              (Colon_Index + 1 .. Name_And_Value'Last);
               begin
                  Process (Name, Value);
               end;
            end if;
         end;
         Start := Index + 1;
         Index := Ada.Strings.Fixed.Index (Source, ";", Start);
      end loop;
   end Scan_Form;

   ----------------------
   -- Get_Symbol_Entry --
   ----------------------

   function Get_Symbol_Entry
     (File    : File_Type;
      Section : Elf_Word_32;
      Index   : Elf_Word_32)
   return Symbol_Table_Entry
   is
      use type System.Storage_Elements.Storage_Offset;
      Rec : Section_Record renames
              File.Section_List
                (Get_Section_Entry (File, Section).Position);
      Base : constant Natural :=
               Natural (Index * Symbol_Table_Entry'Size / 8);
      Data  : System.Storage_Elements.Storage_Array
        (1 .. Symbol_Table_Entry'Size / 8);
      Result : Symbol_Table_Entry;
      for Result'Address use Data'Address;
   begin
      for I in Data'Range loop
         Data (I) := Rec.Data (Base + Natural (I) - 1);
      end loop;
      return Result;
   end Get_Symbol_Entry;

   -----------
   -- Write --
   -----------

   procedure Write
     (File  : in out File_Type;
      Value : System.Storage_Elements.Storage_Element)
   is
   begin
      if File.Saving then
         Storage_IO.Write (File.Storage_File, Value);
      else
         File.Data.Append (Value);
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (File   : in out File_Type;
      Addr   : System.Address;
      Length : System.Storage_Elements.Storage_Count)
   is
      Elements : System.Storage_Elements.Storage_Array (1 .. Length);
      pragma Import (Ada, Elements);
      for Elements'Address use Addr;
   begin
      for Element of Elements loop
         Write (File, Element);
      end loop;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (File   : in out File_Type;
      Vector : Storage_Element_Vectors.Vector)
   is
   begin
      for Element of Vector loop
         Write (File, Element);
      end loop;
   end Write;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name
     (Vector : in out Storage_Element_Vectors.Vector;
      Nm     : String)
   is
   begin
      for Ch of Nm loop
         Vector.Append (Character'Pos (Ch));
      end loop;
      Vector.Append (0);
      while Natural (Vector.Length) mod 4 /= 0 loop
         Vector.Append (0);
      end loop;
   end Write_Name;

end WL.Files.ELF;
