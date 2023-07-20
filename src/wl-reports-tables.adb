package body WL.Reports.Tables is

   ----------------
   -- Add_Column --
   ----------------

   procedure Add_Column
     (Report    : in out Table_Report'Class;
      Heading   : String;
      Alignment : Cell_Alignment := Left)
   is
      pragma Unreferenced (Alignment);
   begin
      Report.Headings.Append (Heading);
   end Add_Column;

   -----------------
   -- Append_Cell --
   -----------------

   procedure Append_Cell
     (Report    : in out Table_Report'Class;
      Value     : String)
   is
   begin
      Report.Rows (Report.Rows.Last).Append (Value);
   end Append_Cell;

   ----------------
   -- Append_Row --
   ----------------

   procedure Append_Row (Report : in out Table_Report'Class) is
   begin
      Report.Rows.Append (String_Vectors.Empty_Vector);
   end Append_Row;

   -------------------
   -- Iterate_Lines --
   -------------------

   overriding procedure Iterate_Lines
     (Report  : Table_Report;
      Process : not null access procedure (Line : String))
   is
      Count  : constant Natural := Report.Headings.Last_Index;
      Length : Natural := 0;
      Width  : array (1 .. Count) of Natural;
      Start  : array (1 .. Count) of Positive := (others => 1);

      function Separator_Line return String;
      function Heading_Line return String;
      function Row_Line (Row : String_Vectors.Vector) return String;

      function Line (Separator : Character;
                     Padding   : Character;
                     Cell_Text : not null access
                       function (Index : Positive) return String)
                     return String;

      ------------------
      -- Heading_Line --
      ------------------

      function Heading_Line return String is
         function Value (Index : Positive) return String
         is (Report.Headings (Index));
      begin
         return Line ('|', ' ', Value'Access);
      end Heading_Line;

      ----------
      -- Line --
      ----------

      function Line (Separator : Character;
                     Padding   : Character;
                     Cell_Text : not null access
                       function (Index : Positive) return String)
                     return String
      is
      begin
         return Line : String (1 .. Length) do
            declare

               Last : Natural := 0;

               procedure Append (Ch : Character);

               ------------
               -- Append --
               ------------

               procedure Append (Ch : Character) is
               begin
                  Last := Last + 1;
                  Line (Last) := Ch;
               end Append;

            begin

               for Col_Index in Width'Range loop
                  Append (Separator);
                  Append (Padding);
                  declare
                     Value : constant String := Cell_Text (Col_Index);
                     Last  : Natural := Value'First - 1;
                  begin
                     for I in 1 .. Width (Col_Index) loop
                        Last := Last + 1;
                        Append (if Last in Value'Range
                                then Value (Last)
                                else Padding);
                     end loop;
                  end;
                  Last := Last + 1;
                  Line (Last) := Padding;
               end loop;
               Append (Separator);
               pragma Assert (Last = Length);
            end;
         end return;
      end Line;

      --------------
      -- Row_Line --
      --------------

      function Row_Line (Row : String_Vectors.Vector) return String is
         function Value (Index : Positive) return String
         is (Row (Index));
      begin
         return Line ('|', ' ', Value'Access);
      end Row_Line;

      --------------------
      -- Separator_Line --
      --------------------

      function Separator_Line return String is

         function Dashes (Count : Natural) return String;

         ------------
         -- Dashes --
         ------------

         function Dashes (Count : Natural) return String is
         begin
            return S : constant String (1 .. Count) := (others => '-');
         end Dashes;

         function Value (Index : Positive) return String
         is (Dashes (Width (Index)));
      begin
         return Line ('+', '-', Value'Access);
      end Separator_Line;

   begin
      if Count = 0 then
         return;
      end if;

      for I in Width'Range loop
         Width (I) := Report.Headings.Element (I)'Length;
      end loop;
      for Row of Report.Rows loop
         for I in 1 .. Natural'Min (Row.Last_Index, Count) loop
            Width (I) := Natural'Max (Width (I), Row.Element (I)'Length);
         end loop;
      end loop;

      for I in 2 .. Count loop
         Start (I) := Start (I - 1) + Width (I - 1) + 3;
      end loop;
      Length := Start (Count) + Width (Count) + 3;

      Process (Separator_Line);
      Process (Heading_Line);
      Process (Separator_Line);
      for Row of Report.Rows loop
         Process (Row_Line (Row));
      end loop;
      Process (Separator_Line);

   end Iterate_Lines;

end WL.Reports.Tables;
