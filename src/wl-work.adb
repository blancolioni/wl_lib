with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

package body WL.Work is

   protected type Handle_Manager is
      procedure Add_Job (Job : Job_Type);
      procedure Job_Finished (Job : Job_Type);
      entry Wait;
   private
      Count : Natural := 0;
   end Handle_Manager;

   type Handle_Manager_Access is access Handle_Manager;

   type Handle_Entry_Record is
      record
         Handle  : Work_Handle;
         Active  : Boolean;
         Manager : Handle_Manager_Access;
      end record;

   type Handle_Entry is access Handle_Entry_Record;

   package Handle_Vectors is
     new Ada.Containers.Vectors (Real_Work_Handle, Handle_Entry);

   protected Handle_Factory is
      procedure New_Handle (Handle : out Work_Handle);
      procedure Close_Handle (Handle : in out Work_Handle);
      function Manager (Handle : Work_Handle) return access Handle_Manager;
   private
      Handles : Handle_Vectors.Vector;
   end Handle_Factory;

   package List_Of_Jobs is
     new Ada.Containers.Doubly_Linked_Lists (Job_Type);

   protected Scheduler is
      function Empty return Boolean;
      procedure Stop;
      procedure Add_Job
        (Job    : Job_Type);
      entry Next_Job
        (Job    : out Job_Type);
   private
      Stopped : Boolean;
      List : List_Of_Jobs.List;
   end Scheduler;

   task type Job_Task;

   type Job_Task_Access is access all Job_Task;

   package Job_Task_Vectors is
     new Ada.Containers.Vectors (Positive, Job_Task_Access);

   Job_Tasks : Job_Task_Vectors.Vector;

   -------------
   -- Add_Job --
   -------------

   procedure Add_Job
     (Handle : Work_Handle;
      Job    : Job_Type)
   is
   begin
      Job.Handle := Handle;
      Handle_Factory.Manager (Handle).Add_Job (Job);
      Scheduler.Add_Job (Job);
   end Add_Job;

   -------------------
   -- Create_Handle --
   -------------------

   function Create_Handle return Work_Handle is
   begin
      return Handle : Work_Handle do
         Handle_Factory.New_Handle (Handle);
      end return;
   end Create_Handle;

   --------------------
   -- Handle_Factory --
   --------------------

   protected body Handle_Factory is

      ------------------
      -- Close_Handle --
      ------------------

      procedure Close_Handle (Handle : in out Work_Handle) is
      begin
         Handles (Handle).Active := False;
      end Close_Handle;

      -------------
      -- Manager --
      -------------

      function Manager (Handle : Work_Handle) return access Handle_Manager is
      begin
         return Handles (Handle).Manager;
      end Manager;

      ----------------
      -- New_Handle --
      ----------------

      procedure New_Handle (Handle : out Work_Handle) is
         Found : Handle_Entry := null;
      begin
         for I in 1 .. Handles.Last_Index loop
            if not Handles (I).Active then
               Found := Handles (I);
               Handle := I;
               exit;
            end if;
         end loop;
         if Found = null then
            Found := new Handle_Entry_Record;
            Found.Manager := new Handle_Manager;
            Handles.Append  (Found);
            Handle := Handles.Last_Index;
         end if;

         Found.Active := True;
         Found.Handle := Handle;

      end New_Handle;

   end Handle_Factory;

   --------------------
   -- Handle_Manager --
   --------------------

   protected body Handle_Manager is

      -------------
      -- Add_Job --
      -------------

      procedure Add_Job (Job : Job_Type) is
         pragma Unreferenced (Job);
      begin
         Count := Count + 1;
      end Add_Job;

      ------------------
      -- Job_Finished --
      ------------------

      procedure Job_Finished (Job : Job_Type) is
         pragma Unreferenced (Job);
      begin
         Count := Count - 1;
      end Job_Finished;

      ----------
      -- Wait --
      ----------

      entry Wait when Count = 0 is
      begin
         null;
      end Wait;

   end Handle_Manager;

   --------------
   -- Job_Task --
   --------------

   task body Job_Task is
      Current : Job_Type;
   begin
      loop
         Scheduler.Next_Job (Current);
         exit when Current = null or else Current.Handle = 0;
         Current.Execute;
         Handle_Factory.Manager (Current.Handle).Job_Finished (Current);
      end loop;
   end Job_Task;

   ---------------
   -- Scheduler --
   ---------------

   protected body Scheduler is

      -------------
      -- Add_Job --
      -------------

      procedure Add_Job
        (Job    : Job_Type)
      is
      begin
         List.Append (Job);
      end Add_Job;

      -----------
      -- Empty --
      -----------

      function Empty return Boolean is
      begin
         return List.Is_Empty;
      end Empty;

      --------------
      -- Next_Job --
      --------------

      entry Next_Job
        (Job    : out Job_Type)
        when not Empty or else Stopped
      is
      begin
         if Stopped then
            Job := null;
         else
            Job := List.First_Element;
            List.Delete_First;
         end if;
      end Next_Job;

      ----------
      -- Stop --
      ----------

      procedure Stop is
      begin
         Stopped := True;
      end Stop;

   end Scheduler;

   --------------------
   -- Set_Task_Count --
   --------------------

   procedure Set_Task_Count (Count : Positive) is
   begin
      Job_Tasks.Clear;
      for I in 1 .. Count loop
         Job_Tasks.Append (new Job_Task);
      end loop;
   end Set_Task_Count;

   ---------------------
   -- Stop_Work_Tasks --
   ---------------------

   procedure Stop_Work_Tasks is
   begin
      Scheduler.Stop;
   end Stop_Work_Tasks;

   ----------
   -- Wait --
   ----------

   procedure Wait (Handle : in out Work_Handle) is
   begin
      Handle_Factory.Manager (Handle).Wait;
      Handle_Factory.Close_Handle (Handle);
   end Wait;

end WL.Work;
