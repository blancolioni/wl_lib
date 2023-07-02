with Ada.Text_IO;

package body WL.Reports is

   ---------
   -- Put --
   ---------

   procedure Put (Report : Report_Interface'Class) is
   begin
      Report.Iterate_Lines (Ada.Text_IO.Put_Line'Access);
   end Put;

end WL.Reports;
