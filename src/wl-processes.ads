private with Ada.Strings.Unbounded;
with Ada.Calendar;

package WL.Processes is

   type Process_Type is tagged limited private;

   procedure Start_Bar
     (Process         :    out Process_Type;
      Name            : String;
      Finish          : Positive;
      With_Percentage : Boolean := False;
      Bar_Length      : Natural  := 40;
      Tick_Size       : Positive := 1);

   procedure Start_Spinner (Process   :    out Process_Type;
                            Name      : String;
                            Tick_Size : Positive := 1);

   procedure Start_Percentage (Process   :    out Process_Type;
                               Name      : String;
                               Finish    : Positive;
                               Tick_Size : Positive := 1);

   procedure Start_Counter (Process   :    out Process_Type;
                            Name      : String;
                            Tick_Size : Positive := 1);

   procedure Tick (Process : in out Process_Type);

   procedure Finish (Process : in out Process_Type);

private

   type Display_Type is (Bar, Spinner, Percentage, Counter);

   type Process_Type is tagged limited
      record
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Display    : Display_Type;
         Prev       : Ada.Calendar.Time;
         Percent    : Boolean  := False;
         Last_Value : Natural  := 0;
         Bar_Length : Natural  := 0;
         Finish     : Natural  := 0;
         Tick       : Natural  := 0;
         Step       : Positive := 1;
         Acc        : Positive := 1;
      end record;

end WL.Processes;
