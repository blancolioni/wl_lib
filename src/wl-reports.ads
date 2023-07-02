package WL.Reports is

   type Report_Interface is interface;

   procedure Iterate_Lines
     (Report  : Report_Interface;
      Process : not null access
        procedure (Line : String))
   is abstract;

   procedure Put (Report : Report_Interface'Class);

end WL.Reports;
