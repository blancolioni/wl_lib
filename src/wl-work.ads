package WL.Work is

   procedure Set_Task_Count (Count : Positive);
   procedure Stop_Work_Tasks;

   type Root_Job_Type is abstract tagged private;

   procedure Execute (Job : in out Root_Job_Type) is abstract;

   type Job_Type is access all Root_Job_Type'Class;

   type Work_Handle is limited private;

   function Create_Handle return Work_Handle;

   procedure Add_Job
     (Handle : Work_Handle;
      Job    : Job_Type);

   procedure Wait (Handle : in out Work_Handle);

private

   type Work_Handle is new Natural;

   subtype Real_Work_Handle is Work_Handle range 1 .. Work_Handle'Last;

   type Root_Job_Type is abstract tagged
      record
         Handle : Work_Handle := 0;
      end record;

end WL.Work;
