private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Vectors;

package WL.Reports.Tables is

   type Cell_Alignment is (Left, Center, Right);

   type Table_Report is
     new Report_Interface with private;

   procedure Add_Column
     (Report    : in out Table_Report'Class;
      Heading   : String;
      Alignment : Cell_Alignment := Left);

   procedure Append_Row
     (Report : in out Table_Report'Class);

   procedure Append_Cell
     (Report    : in out Table_Report'Class;
      Value     : String);

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Row_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (String_Vectors.Vector, String_Vectors."=");

   type Table_Report is new Report_Interface with
      record
         Headings : String_Vectors.Vector;
         Rows     : Row_Lists.List;
      end record;

   overriding procedure Iterate_Lines
     (Report  : Table_Report;
      Process : not null access
        procedure (Line : String));

end WL.Reports.Tables;
