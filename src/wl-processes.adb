with Ada.Integer_Text_IO;
with Ada.Text_IO;                       use Ada.Text_IO;

package body WL.Processes is

   Send_Escape_Sequences : constant Boolean := True;

   Progress_Characters : constant String :=
                           "-\|/";

   type Escape_Sequence is array (Positive range <>) of Natural;

   Cursor_On : constant Escape_Sequence :=
                 (16#1B#, 16#5B#, 16#3F#, 16#32#, 16#35#, 16#68#);
   Cursor_Off : constant Escape_Sequence :=
                  (16#1B#, 16#5B#, 16#3F#, 16#32#, 16#35#, 16#6C#);

   procedure Send_Escape_Sequence
     (Sequence : Escape_Sequence);

   function "+" (X : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (X : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;
   ------------
   -- Finish --
   ------------

   procedure Finish (Process : in out Process_Type) is
   begin
      case Process.Display is
         when Spinner =>
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             Character'Val (8) & ' ');
            Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
         when Bar =>
            declare
               Count  : constant Natural := Process.Bar_Length;
               Final  : constant String (1 .. Count) :=
                          (others => '=');
            begin
               Put
                 (Ada.Text_IO.Standard_Error,
                  Character'Val (13)
                  & (-Process.Name)
                  & ": ");
               if Process.Percent then
                  Put (Standard_Error, "100% ");
               end if;
               Put
                 (Ada.Text_IO.Standard_Error,
                  "[" & Final & "]");
            end;
         when Percentage =>
            Put (Ada.Text_IO.Standard_Error,
                 Character'Val (13) &
                   (-Process.Name) & ": 100%");
         when Counter =>
            Put (Ada.Text_IO.Standard_Error,
                 Character'Val (13)
                 & (-Process.Name) & ":"
                 & Natural'Image (Process.Tick));
      end case;

      Process.Tick := 0;
      New_Line (Ada.Text_IO.Standard_Error);
      Send_Escape_Sequence (Cursor_On);
   exception
      when others =>
         Send_Escape_Sequence (Cursor_On);
         raise;
   end Finish;

   --------------------------
   -- Send_Escape_Sequence --
   --------------------------

   procedure Send_Escape_Sequence
     (Sequence : Escape_Sequence)
   is
   begin
      if Send_Escape_Sequences then
         for Code of Sequence loop
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             Character'Val (Code));
         end loop;
      end if;
   end Send_Escape_Sequence;

   ---------------
   -- Start_Bar --
   ---------------

   procedure Start_Bar
     (Process         :    out Process_Type;
      Name            : String;
      Finish          : Positive;
      With_Percentage : Boolean := False;
      Bar_Length      : Natural  := 40;
      Tick_Size       : Positive := 1)
   is
      Spaces : constant String (1 .. Process.Bar_Length) :=
                 (others => ' ');
   begin
      Send_Escape_Sequence (Cursor_Off);
      Put (Ada.Text_IO.Standard_Error,
           Name & ":      [" & Spaces & "]");
      Flush (Ada.Text_IO.Standard_Error);
      Process.Name    := +Name;
      Process.Display := Bar;
      Process.Percent := With_Percentage;
      Process.Tick    := 0;
      Process.Finish  := Finish;
      Process.Step    := Tick_Size;
      Process.Acc     := 1;
      Process.Bar_Length := Natural'Min (Bar_Length, Finish);
   end Start_Bar;

   -------------------
   -- Start_Counter --
   -------------------

   procedure Start_Counter (Process   :    out Process_Type;
                            Name      : String;
                            Tick_Size : Positive := 1)
   is
   begin
      Put (Ada.Text_IO.Standard_Error, Name & ": 0");
      Flush (Ada.Text_IO.Standard_Error);
      Process.Name    := +Name;
      Process.Display := Counter;
      Process.Tick    := 0;
      Process.Step    := Tick_Size;
      Process.Prev    := Ada.Calendar.Clock;
      Send_Escape_Sequence (Cursor_Off);
   end Start_Counter;

   ----------------------
   -- Start_Percentage --
   ----------------------

   procedure Start_Percentage
     (Process   :    out Process_Type;
      Name      : String;
      Finish    : Positive;
      Tick_Size : Positive := 1)
   is
   begin
      Put (Ada.Text_IO.Standard_Error, Name & ": 0%");
      Flush (Ada.Text_IO.Standard_Error);
      Process.Name    := +Name;
      Process.Display := Percentage;
      Process.Tick    := 0;
      Process.Finish  := Finish;
      Process.Step    := Tick_Size;
      Process.Acc     := 1;
      Send_Escape_Sequence (Cursor_Off);
   end Start_Percentage;

   -------------------
   -- Start_Spinner --
   -------------------

   procedure Start_Spinner
     (Process   :    out Process_Type;
      Name      : String;
      Tick_Size : Positive := 1)
   is
   begin
      Put (Ada.Text_IO.Standard_Error, Name & ": " &
           Progress_Characters (1));
      Flush (Ada.Text_IO.Standard_Error);
      Process.Display := Spinner;
      Process.Tick    := 0;
      Process.Step    := Tick_Size;
      Process.Acc     := 1;
      Send_Escape_Sequence (Cursor_Off);
   end Start_Spinner;

   ----------
   -- Tick --
   ----------

   procedure Tick (Process : in out Process_Type) is
   begin
      Process.Tick := Process.Tick + 1;
      if Process.Tick > Process.Finish then
         Process.Tick := Process.Finish;
      end if;

      if Process.Tick mod Process.Step = 0 then

         case Process.Display is
            when Spinner =>
               Process.Acc := Process.Acc + 1;
               if Process.Acc > Progress_Characters'Last then
                  Process.Acc := 1;
               end if;

               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                                Character'Val (8) &
                                  Progress_Characters (Process.Acc));
               Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);

            when Bar =>
               declare
                  Count    : constant Natural :=
                               Process.Finish / Process.Step;
                  Current  : constant Natural :=
                               Process.Tick / Process.Step;
                  Bar_Step : constant Natural :=
                               Count / Process.Bar_Length;
                  Partial  : constant Natural :=
                               Process.Tick mod Bar_Step;
                  Filled_Length : constant Natural :=
                                    Current * Process.Bar_Length
                                      / Count;
                  Half_Length   : constant Natural :=
                                    (if Partial > Bar_Step / 2
                                     and then Current < Count
                                     then 1 else 0);
                  Empty_Length  : constant Natural :=
                                    Process.Bar_Length
                                      - Filled_Length - Half_Length;
                  Filled        : constant String (1 .. Filled_Length) :=
                                    (others => '=');
                  Half          : constant String (1 .. Half_Length) :=
                                    (others => '-');
                  Empty         : constant String (1 .. Empty_Length) :=
                                    (others => ' ');
                  Bar           : constant String := Filled & Half & Empty;

               begin
                  if Current * 2 + Half_Length /= Process.Last_Value then
                     Put
                       (Ada.Text_IO.Standard_Error,
                        Character'Val (13)
                        & (-Process.Name)
                        & ": ");
                     if Process.Percent then
                        Ada.Integer_Text_IO.Put
                          (Standard_Error,
                           100 * Current / Count, 3);
                        Ada.Text_IO.Put
                          (Standard_Error,
                           "% ");
                     end if;
                     Ada.Text_IO.Put
                       (Standard_Error,
                        "[" & Bar & "]");
                     Process.Last_Value := Current * 2 + Half_Length;
                     Flush (Ada.Text_IO.Standard_Error);
                  end if;
               end;
            when Percentage =>
               declare
                  Last_Value : constant Natural :=
                                 Natural (Float (Process.Tick - 1)
                                          / Float (Process.Finish) * 100.0);
                  Value : constant Natural :=
                                 Natural (Float (Process.Tick)
                                          / Float (Process.Finish) * 100.0);
               begin
                  if Last_Value /= Value then
                     Put (Ada.Text_IO.Standard_Error,
                          Character'Val (13) &
                            (-Process.Name) & ":"
                          & Natural'Image (Value) & "%");
                     Flush (Ada.Text_IO.Standard_Error);
                  end if;
               end;
            when Counter =>
               declare
                  use Ada.Calendar;
                  Now : constant Time := Clock;
               begin
                  if Now - Process.Prev > 0.1 then
                     Put (Ada.Text_IO.Standard_Error,
                          Character'Val (13)
                          & (-Process.Name) & ":"
                          & Natural'Image (Process.Tick));
                     Flush (Ada.Text_IO.Standard_Error);
                     Process.Prev := Now;
                  end if;
               end;
         end case;
      end if;
   exception
      when others =>
         Send_Escape_Sequence (Cursor_On);
         raise;
   end Tick;

end WL.Processes;
