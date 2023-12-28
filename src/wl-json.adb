with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Tags;
with Ada.Text_IO;

package body WL.Json is

   type Null_Json_Value is
     new Atomic_Json_Value with null record;

   overriding function Serialize
     (Value : Null_Json_Value)
      return String;

   type String_Json_Value is
     new Atomic_Json_Value with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Serialize
     (Value : String_Json_Value)
      return String;

   overriding function Image
     (Value : String_Json_Value)
      return String
   is (Ada.Strings.Unbounded.To_String (Value.Text));

   type Integer_Json_Value is
     new Atomic_Json_Value with
      record
         Value : Integer;
      end record;

   overriding function Serialize
     (Value : Integer_Json_Value)
      return String;

   type Float_Json_Value is
     new Atomic_Json_Value with
      record
         Value : Float;
      end record;

   overriding function Serialize
     (Value : Float_Json_Value)
      return String;

   type Boolean_Json_Value is
     new Atomic_Json_Value with
      record
         Value : Boolean;
      end record;

   overriding function Serialize
     (Value : Boolean_Json_Value)
      return String;

   function To_Safe_String (Text : String) return String;

   function Parse_String
     (S : String)
      return Json_Value'Class
      is separate;

   ------------
   -- Append --
   ------------

   procedure Append
     (To : in out Json_Array'Class; Value : Json_Value'Class) is
   begin
      To.Vector.Append (Value);
   end Append;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value (Bool : Boolean) return Json_Value'Class is
   begin
      return Result : constant Boolean_Json_Value :=
        Boolean_Json_Value'
          (Value => Bool);
   end Boolean_Value;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (To   : in out Json_Object'Class;
      From : Json_Object'Class)
   is
   begin
      for Position in From.Properties.Iterate loop
         To.Set_Property
           (Json_Value_Maps.Key (Position),
            Json_Value_Maps.Element (Position));
      end loop;
   end Copy;

   -----------------
   -- Float_Value --
   -----------------

   function Float_Value (F : Float) return Json_Value'Class is
   begin
      return Result : constant Float_Json_Value := Float_Json_Value'
        (Value => F);
   end Float_Value;

   -----------------
   -- Deserialize --
   -----------------

   function Deserialize
     (Text : String)
      return Json_Value'Class
   is
   begin
      return Parse_String (Text);
   end Deserialize;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Value : Json_Value;
      Name  : String)
      return Json_Value'Class
   is
   begin
      if Name = "_toString" then
         return String_Value (Json_Value'Class (Value).Image);
      else
         return Null_Value;
      end if;
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Object : Json_Object;
      Name   : String)
      return Json_Value'Class
   is
   begin
      if Object.Properties.Contains (Name) then
         return Object.Properties (Name);
      else
         return Json_Value (Object).Get_Property (Name);
      end if;
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Value  : Json_Value'Class;
      Name   : String)
      return String
   is
   begin
      return Value.Get_Property (Name).Image;
   end Get_Property;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value (Int : Integer) return Json_Value'Class is
   begin
      return Result : constant Integer_Json_Value := Integer_Json_Value'
        (Value => Int);
   end Integer_Value;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Value : Json_Value'Class) return Boolean is
   begin
      return Value in Null_Json_Value'Class;
   end Is_Null;

   ----------------
   -- Null_Value --
   ----------------

   function Null_Value return Json_Value'Class is
   begin
      return Result : Null_Json_Value;
   end Null_Value;

   ---------------
   -- Serialize --
   ---------------

   overriding function Serialize (Value : Json_Object) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Position in Value.Properties.Iterate loop
         if Result /= Null_Unbounded_String then
            Result := Result & ",";
         end if;
         Result := Result & """" & Json_Value_Maps.Key (Position)
           & """:" & Json_Value_Maps.Element (Position).Serialize;
      end loop;
      return "{" & To_String (Result) & "}";
   end Serialize;

   ---------------
   -- Serialize --
   ---------------

   overriding function Serialize (Value : Json_Array) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Item of Value.Vector loop
         if Result /= Null_Unbounded_String then
            Result := Result & ",";
         end if;
         Result := Result & Item.Serialize;
      end loop;
      return "[" & To_String (Result) & "]";
   end Serialize;

   overriding function Serialize
     (Value : Null_Json_Value)
      return String
   is ("null");

   overriding function Serialize
     (Value : String_Json_Value)
      return String
   is (""""
       & To_Safe_String (Ada.Strings.Unbounded.To_String (Value.Text))
       & """");

   overriding function Serialize
     (Value : Integer_Json_Value)
      return String
   is (Ada.Strings.Fixed.Trim (Value.Value'Image, Ada.Strings.Left));

   overriding function Serialize
     (Value : Float_Json_Value)
      return String
   is (Ada.Strings.Fixed.Trim (Value.Value'Image, Ada.Strings.Left));

   overriding function Serialize
     (Value : Boolean_Json_Value)
      return String
   is (if Value.Value then "true" else "false");

   ----------
   -- Save --
   ----------

   procedure Save
     (Value : Json_Value'Class;
      Path  : String)
   is
      use Ada.Text_IO;
      File : File_Type;

      procedure Write_Indented
        (This   : Json_Value'Class;
         Indent : Positive_Count);

      procedure Write_String
        (Text : String);

      --------------------
      -- Write_Indented --
      --------------------

      procedure Write_Indented
        (This   : Json_Value'Class;
         Indent : Positive_Count)
      is
         Child_Indent : constant Positive_Count := Indent + 4;
      begin
         if This in Atomic_Json_Value'Class then
            Put (This.Serialize);
         elsif This in Json_Array'Class then
            Put_Line ("[");
            declare
               First : Boolean := True;
            begin
               for Value of Json_Array (This).Vector loop
                  if First then
                     First := False;
                  else
                     Put_Line (",");
                  end if;
                  Set_Col (Child_Indent);
                  Write_Indented (Value, Child_Indent);
               end loop;
            end;
            New_Line;
            Set_Col (Indent);
            Put ("]");
         elsif This in Json_Object'Class then
            Put_Line ("{");
            declare
               First : Boolean := True;
            begin
               for Position in Json_Object (This).Properties.Iterate loop
                  if First then
                     First := False;
                  else
                     Put_Line (",");
                  end if;
                  Set_Col (Child_Indent);
                  Write_String (Json_Value_Maps.Key (Position));
                  Put (": ");
                  Write_Indented
                    (Json_Value_Maps.Element (Position), Child_Indent);
               end loop;
            end;
            New_Line;
            Set_Col (Indent);
            Put ("}");
         else
            raise Program_Error with
              "unknown json value type: "
              & Ada.Tags.External_Tag (This'Tag);
         end if;
      end Write_Indented;

      ------------------
      -- Write_String --
      ------------------

      procedure Write_String
        (Text : String)
      is
         Image : String (1 .. Text'Length * 2);
         Last  : Natural := 0;
      begin
         for Ch of Text loop
            Last := Last + 1;
            if Ch in '"' | '\' then
               Image (Last) := '\';
               Last := Last + 1;
            end if;
            Image (Last) := Ch;
         end loop;
         Put ('"' & Image (1 .. Last) & '"');
      end Write_String;

   begin
      Create (File, Out_File, Path);
      Set_Output (File);
      Write_Indented (Value, 1);
      Set_Output (Standard_Output);
      Close (File);
   end Save;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Json_Value'Class)
   is
   begin
      if Object.Properties.Contains (Name) then
         Object.Properties.Replace (Name, Value);
      else
         Object.Properties.Insert (Name, Value);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : String)
   is
   begin
      Object.Set_Property (Name, String_Value (Value));
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Integer)
   is
   begin
      Object.Set_Property (Name, Integer_Value (Value));
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Float)
   is
   begin
      Object.Set_Property (Name, Float_Value (Value));
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Boolean)
   is
   begin
      Object.Set_Property (Name, Boolean_Value (Value));
   end Set_Property;

   ------------------
   -- String_Value --
   ------------------

   function String_Value (Text : String) return Json_Value'Class is
   begin
      return Result : constant String_Json_Value := String_Json_Value'
        (Text =>
           Ada.Strings.Unbounded.To_Unbounded_String
             (Text));
   end String_Value;

   --------------------
   -- To_Safe_String --
   --------------------

   function To_Safe_String (Text : String) return String is
   begin
      for I in Text'Range loop
         if Character'Pos (Text (I)) = 10 then
            return Text (Text'First .. I - 1)
              & "\n"
              & To_Safe_String (Text (I + 1 .. Text'Last));
         elsif Text (I) = '"' then
            return Text (Text'First .. I - 1)
              & "\"
              & Text (I)
              & To_Safe_String (Text (I + 1 .. Text'Last));
         end if;
      end loop;
      return Text;
   end To_Safe_String;

end WL.Json;
