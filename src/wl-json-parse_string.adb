with Ada.Characters.Handling;
with Ada.Text_IO;

separate (WL.Json)
function Parse_String (S : String) return Json_Value'Class is
   Current_Line_Index : Natural := 1;
   Current_Last       : Natural := 0;
   Current_Index      : Natural := S'First - 1;
   Current_Character  : Character := ' ';
   Done               : Boolean := False;

   procedure Start;
   procedure Finish;

   procedure Next_Character;

   procedure Skip_Whitespace;

   procedure Error (Message : String);

   function Parse_Json_Element return Json_Value'Class;
   function Parse_Json_Object return Json_Value'Class;
   function Parse_Json_Array return Json_Value'Class;

   function Parse_Terminal return Json_Value'Class;
   function Parse_Rest_Of_String return String;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "error:"
         & Current_Line_Index'Image
         & ":"
         & Current_Last'Image
         & ": " & Message);

      raise Constraint_Error with Message;
   end Error;

   ------------
   -- Finish --
   ------------

   procedure Finish is
   begin
      Skip_Whitespace;
      if not Done then
         Error ("extra ignored");
      end if;
   end Finish;

   --------------------
   -- Next_Character --
   --------------------

   procedure Next_Character is
   begin
      if Current_Index >= S'Last then
         Done := True;
      else
         Current_Index := Current_Index + 1;
         Current_Character := S (Current_Index);
      end if;
   end Next_Character;

   ----------------------
   -- Parse_Json_Array --
   ----------------------

   function Parse_Json_Array return Json_Value'Class is
      This : Json_Array;
   begin
      while not Done
        and then Current_Character /= ']'
      loop
         This.Append (Parse_Json_Element);
         Skip_Whitespace;

         if Current_Character = ',' then
            Next_Character;
            Skip_Whitespace;
         elsif Current_Character /= ']' then
            Error ("missing ',' in array");
         end if;

      end loop;

      if Current_Character = ']' then
         Next_Character;
      else
         Error ("missing ']'");
      end if;

      return This;

   end Parse_Json_Array;

   ------------------------
   -- Parse_Json_Element --
   ------------------------

   function Parse_Json_Element return Json_Value'Class is
   begin
      Skip_Whitespace;

      if Done then
         Error ("expected a json element");
      end if;

      case Current_Character is
         when '{' =>
            Next_Character;
            Skip_Whitespace;
            return Parse_Json_Object;
         when '[' =>
            Next_Character;
            Skip_Whitespace;
            return Parse_Json_Array;
         when others =>
            return Parse_Terminal;
      end case;
   end Parse_Json_Element;

   -----------------------
   -- Parse_Json_Object --
   -----------------------

   function Parse_Json_Object return Json_Value'Class is
      This : Json_Object;
   begin
      while not Done
        and then Current_Character /= '}'
      loop
         case Current_Character is
            when '"' =>
               Next_Character;
               declare
                  Id : constant String := Parse_Rest_Of_String;
               begin
                  Skip_Whitespace;
                  if Current_Character = ':' then
                     Next_Character;
                  else
                     Ada.Text_IO.Put_Line (S);
                     Error ("missing value at " & Current_Character);
                  end if;

                  This.Set_Property (Id, Parse_Json_Element);
               end;
            when others =>
               Error ("missing field name");
         end case;

         Skip_Whitespace;

         if Current_Character = ',' then
            Next_Character;
         elsif Current_Character = '}' then
            null;
         else
            Error ("missing ','");
         end if;

         Skip_Whitespace;

      end loop;

      if Current_Character = '}' then
         Next_Character;
      else
         Error ("missing close brace");
      end if;

      return This;

   end Parse_Json_Object;

   --------------------------
   -- Parse_Rest_Of_String --
   --------------------------

   function Parse_Rest_Of_String return String is
      Start  : constant Positive := Current_Index;
      Finish : Natural  := Current_Index - 1;
   begin
      while not Done and then Current_Character /= '"' loop
         if Current_Character = '\' then
            Next_Character;
         end if;
         Finish := Current_Index;
         Next_Character;
      end loop;

      if Done then
         Error ("missing close quote");
      end if;

      Next_Character;

      declare
         Result : String (1 .. Finish - Start + 1);
         Last   : Natural := 0;
      begin
         for Ch of S (Start .. Finish) loop
            if Ch /= '\' then
               Last := Last + 1;
               Result (Last) := Ch;
            end if;
         end loop;
         return Result;
      end;
   end Parse_Rest_Of_String;

   --------------------
   -- Parse_Terminal --
   --------------------

   function Parse_Terminal return Json_Value'Class is

      Start  : constant Positive := Current_Index;
      Finish : Natural  := Current_Index;

   begin
      if Current_Character = '"' then
         Next_Character;
         declare
            Result : constant String :=
                       Parse_Rest_Of_String;
         begin
            return String_Value (Result);
         end;
      elsif Current_Character in '0' .. '9' | '+' | '-' then
         declare
            Is_Float : Boolean := False;
         begin
            if Current_Character in '-' | '+' then
               Next_Character;
            end if;

            while not Done
              and then Current_Character in
                '0' .. '9' | '.' | 'e' | 'E' | '+' | '-'
            loop
               if Current_Character in '.' | 'e' | 'E' then
                  Is_Float := True;
               end if;
               Next_Character;
            end loop;

            Finish := Current_Index - (if Done then 0 else 1);

            declare
               Img : constant String :=
                       S (Start .. Finish);
            begin
               if Is_Float then
                  return Float_Value (Float'Value (Img));
               else
                  return Integer_Value (Integer'Value (Img));
               end if;
            exception
               when Constraint_Error =>
                  if Is_Float then
                     Error ("invalid floating point constant");
                  else
                     Error ("invalid integer constant");
                  end if;
                  return Null_Value;
            end;
         end;

      elsif Ada.Characters.Handling.Is_Letter (Current_Character) then
         while not Done
           and then Ada.Characters.Handling.Is_Alphanumeric
             (Current_Character)
         loop
            Next_Character;
         end loop;

         Finish := Current_Index - (if Done then 0 else 1);

         declare
            Token : constant String := S (Start .. Finish);
         begin
            if Token = "null" then
               return Null_Value;
            elsif Token = "true" then
               return Boolean_Value (True);
            elsif Token = "false" then
               return Boolean_Value (False);
            else
               Error ("expected a name at [" & Token & "]");
               return Null_Value;
            end if;
         end;
      else
         Error ("invalid character");
         return Null_Value;
      end if;

   end Parse_Terminal;

   ---------------------
   -- Skip_Whitespace --
   ---------------------

   procedure Skip_Whitespace is
   begin
      while not Done
        and then (Ada.Characters.Handling.Is_Space (Current_Character)
                  or else Current_Character = Character'Val (9)
                  or else Current_Character = Character'Val (10)
                  or else Current_Character = Character'Val (13))
      loop
         if Current_Character = Character'Val (10) then
            Current_Line_Index := Current_Line_Index + 1;
         end if;
         Next_Character;
      end loop;
      Current_Last := Current_Index;
   end Skip_Whitespace;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Next_Character;
   end Start;

begin
   Start;
   declare
      Result : constant Json_Value'Class := Parse_Json_Element;
   begin
      Finish;
      return Result;
   end;
end Parse_String;
