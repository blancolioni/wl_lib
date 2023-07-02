package WL.Command_Line is

   procedure Load_Defaults
     (File_Path : String);

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character)
     return Boolean;

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character)
     return String;

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character;
      Default    : Integer    := 0)
     return Integer;

   function Argument_Count return Natural;
   function Argument (Index : Positive) return String
     with Pre => Index <= Argument_Count;

end WL.Command_Line;
