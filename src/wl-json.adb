with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body WL.Json is

   type Null_Json_Value is
     new Json_Value with null record;

   overriding function Serialize
     (Value : Null_Json_Value)
      return String;

   type String_Json_Value is
     new Json_Value with
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
     new Json_Value with
      record
         Value : Integer;
      end record;

   overriding function Serialize
     (Value : Integer_Json_Value)
      return String;

   type Float_Json_Value is
     new Json_Value with
      record
         Value : Float;
      end record;

   overriding function Serialize
     (Value : Float_Json_Value)
      return String;

   type Boolean_Json_Value is
     new Json_Value with
      record
         Value : Boolean;
      end record;

   overriding function Serialize
     (Value : Boolean_Json_Value)
      return String;

   function To_Safe_String (Text : String) return String;

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
   -- Deserialize --
   -----------------

   function Deserialize
     (Text : String)
      return Json_Value'Class
   is
      use Ada.Characters.Handling;

      Index : Positive := Text'First;
      Eos   : Boolean := Text = "";

      procedure Next;

      function Current return Character
      is (if Index in Text'Range then Text (Index) else Character'Val (4));

      function At_Space return Boolean
      is (Ada.Characters.Handling.Is_Space (Current));

      function Next_Token return String;

      procedure Error (Message : String);
      procedure Expect (Tok : String);

      function Parse_Value
        (Start : String)
         return Json_Value'Class;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         raise Constraint_Error with Message;
      end Error;

      ------------
      -- Expect --
      ------------

      procedure Expect (Tok : String) is
         Check : constant String := Next_Token;
      begin
         if Check /= Tok then
            raise Constraint_Error with
              "expected " & Tok & " but found " & Check;
         end if;
      end Expect;

      ----------
      -- Next --
      ----------

      procedure Next is
      begin
         Index := Index + 1;
         Eos := Index > Text'Last;
      end Next;

      ----------------
      -- Next_Token --
      ----------------

      function Next_Token return String is
      begin
         while not Eos and then At_Space loop
            Next;
         end loop;

         if Eos then
            return "";
         end if;

         if Current = '"' then
            declare
               Start : constant Positive := Index;
            begin
               Next;
               while not Eos and then Current /= '"' loop
                  if Current = '\' then
                     Next;
                  end if;
                  Next;
               end loop;

               if Eos then
                  raise Constraint_Error with
                    "unterminated string";
               end if;

               declare
                  Last : constant Positive := Index;
               begin
                  Next;
                  return Text (Start .. Last);
               end;
            end;
         elsif Current in '0' .. '9' then
            declare
               Start : constant Positive := Index;
            begin
               Next;
               while not Eos and then Current in '0' .. '9' loop
                  Next;
               end loop;
               return Text (Start .. Index - 1);
            end;
         elsif Is_Letter (Current) then
            declare
               Start : constant Positive := Index;
            begin
               while not Eos and then Is_Alphanumeric (Current) loop
                  Next;
               end loop;

               declare
                  Id : constant String := Text (Start .. Index - 1);
               begin
                  if Id = "true" or else Id = "false"
                    or else Id = "null"
                  then
                     return Id;
                  else
                     raise Constraint_Error with
                       "invalid identifier";
                  end if;
               end;
            end;
         elsif Current in '{' | '}' | '[' | ']' | ',' | ':' then
            declare
               Result : constant String := (1 => Current);
            begin
               Next;
               return Result;
            end;
         else
            raise Constraint_Error with
              "bad character: [" & Current & "]";
         end if;
      end Next_Token;

      -----------------
      -- Parse_Value --
      -----------------

      function Parse_Value
        (Start : String)
         return Json_Value'Class
      is
      begin
         if Start = "true" or else Start = "false" then
            return Boolean_Value (Boolean'Value (Start));
         elsif Start = "" or else Start = "null" then
            return Null_Value;
         elsif Start (Start'First) in '0' .. '9' | '+' | '-' then
            return Integer_Value (Integer'Value (Start));
         elsif Start (Start'First) = '"' then
            return String_Value (Start (Start'First + 1 .. Start'Last - 1));
         elsif Start = "[" then
            declare
               Arr : Json_Array;
            begin
               loop
                  if Eos then
                     raise Constraint_Error with
                       "unterminated array";
                  end if;

                  declare
                     Tok : constant String := Next_Token;
                  begin
                     if Tok = "]" then
                        return Arr;
                     else
                        Arr.Append (Parse_Value (Tok));
                        if Tok = "]" then
                           return Arr;
                        elsif Tok /= "," then
                           raise Constraint_Error
                             with "invalid array";
                        end if;
                     end if;
                  end;
               end loop;
            end;
         elsif Start = "{" then
            declare
               Object : Json_Object;
            begin
               loop
                  if Eos then
                     raise Constraint_Error with
                       "unterminated object";
                  end if;

                  declare
                     Tok : constant String := Next_Token;
                  begin
                     if Tok = "}" then
                        return Object;
                     elsif Tok = "" or else Tok (Tok'First) /= '"' then
                        Error ("missing property name");
                     else
                        declare
                           Prop_Name : constant String :=
                             Tok (Tok'First + 1 .. Tok'Last - 1);
                        begin
                           Expect (":");

                           Object.Set_Property
                             (Prop_Name, Parse_Value (Next_Token));

                           declare
                              Sep : constant String := Next_Token;
                           begin
                              if Sep = "}" then
                                 return Object;
                              elsif Sep /= "," then
                                 Error ("missing ',' or '}'");
                              end if;
                           end;
                        end;
                     end if;
                  end;
               end loop;
            end;
         else
            raise Constraint_Error with
              "syntax error at [" & Start & "]";
         end if;
      end Parse_Value;

   begin
      return Parse_Value (Next_Token);
   end Deserialize;

   -----------------
   -- Float_Value --
   -----------------

   function Float_Value (F : Float) return Json_Value'Class is
   begin
      return Result : constant Float_Json_Value := Float_Json_Value'
        (Value => F);
   end Float_Value;

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
