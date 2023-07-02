with Ada.Strings.Fixed;

package body WL.Files.ELF is

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
     (File : in out File_Type;
      Alignment : System.Storage_Elements.Storage_Count);

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
                    Sh_Type      => Section_Header_Type'Pos (Rel),
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
         Rel : Relocation_Entry :=
                 Relocation_Entry'
                   (Offset => Section_Offset,
                    Info   => (File.Symbol_Map (Name) - 1) * 256
                    + Elf_Word_32 (Info));
         Data : Storage_Array (1 .. Rel'Size / 8);
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

      for Section of File.Section_List loop
         if Section.Header.Sh_Type = Section_Header_Type'Pos (Rel) then
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
              Sh_Type      => Section_Header_Type'Pos (Symtab),
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
              Sh_Type      => Section_Header_Type'Pos (Strtab),
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
      function Flag (B : Boolean;
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
      Sym : constant Symbol_Table_Entry :=
              Symbol_Table_Entry'
                (St_Name    => Save_Name (File, Name),
                 St_Value   => Value,
                 St_Size    => Elf_Word_32 (Size),
                 St_Info    => Symbol_Table_Binding'Pos (Binding) * 2 ** 4
                 + Symbol_Table_Type'Pos (Typ) mod 16,
                 St_Other   => Symbol_Table_Visibility'Pos (Visibility),
                 St_Shndx   => Shndx);
      Data : Storage_Array (1 .. Sym'Size / 8);
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
   begin
      null;
   end Open;

   ---------
   -- Put --
   ---------

   procedure Put (File : in out File_Type; Value : Octet) is
   begin
      File.Section_List (File.Section_List.Last)
        .Data.Append (System.Storage_Elements.Storage_Element (Value));
   end Put;

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
                  Name : constant String :=
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
