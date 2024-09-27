private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Strings.Unbounded;

package WL.Unit is

   type Test_Result (<>) is private;

   function Test_Success return Test_Result;
   --  @return a result which indicates the test was successful

   function Test_Failure
     (Expected : String;
      Found    : String;
      Message  : String)
      return Test_Result;
   --  Construct and return a test result value indicating
   --  a failed test.
   --  @param Expected expected result of the test
   --  @param Found found result of the test
   --  @return a Test_Result which indicates a failed test.

   function Test_Error
     (Message : String)
      return Test_Result;
   --  Construct and return a test result value indicating that
   --  an error occurred while running the test
   --  @param Message an error message, for example the string
   --  raise with an exception.
   --  @return a test result object indicating an error.

   function Is_Success (This : Test_Result) return Boolean;
   function Is_Failure (This : Test_Result) return Boolean;
   function Is_Error (This : Test_Result) return Boolean;

   function Expected (This : Test_Result) return String
     with Pre => Is_Failure (This);
   function Found (This : Test_Result) return String
     with Pre => Is_Failure (This);

   function Error_Message (This : Test_Result) return String
     with Pre => Is_Error (This);

   type Unit_Test is abstract tagged private;

   function Name (This : Unit_Test) return String is abstract;

   function Try
     (This : Unit_Test)
      return Test_Result
      is abstract;

   type Test_Suite is tagged private;

   procedure Append
     (To   : in out Test_Suite;
      Test : Unit_Test'Class);

   procedure Verbose
     (Suite   : in out Test_Suite;
      Enabled : Boolean := True);

   procedure Verbose_Errors
     (Suite   : in out Test_Suite;
      Enabled : Boolean := True);

   procedure Run_Tests
     (Suite   : in out Test_Suite;
      Success : out Natural;
      Failure : out Natural;
      Error   : out Natural;
      Not_Run : out Natural);

private

   type Test_Result_Type is (Success, Failure, Error);

   type Test_Result (Result : Test_Result_Type) is
      record
         case Result is
            when Success =>
               null;
            when Failure =>
               Expected : Ada.Strings.Unbounded.Unbounded_String;
               Found    : Ada.Strings.Unbounded.Unbounded_String;
               Cause    : Ada.Strings.Unbounded.Unbounded_String;
            when Error =>
               Message  : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   function Test_Success return Test_Result
   is (Result => Success);

   function Test_Failure
     (Expected : String;
      Found    : String;
      Message  : String)
      return Test_Result
   is (Result   => Failure,
       Expected => Ada.Strings.Unbounded.To_Unbounded_String (Expected),
       Found    => Ada.Strings.Unbounded.To_Unbounded_String (Found),
       Cause    => Ada.Strings.Unbounded.To_Unbounded_String (Message));

   function Test_Error
     (Message : String)
      return Test_Result
   is (Result  => Error,
       Message => Ada.Strings.Unbounded.To_Unbounded_String (Message));

   function Is_Success (This : Test_Result) return Boolean
   is (This.Result = Success);

   function Is_Failure (This : Test_Result) return Boolean
   is (This.Result = Failure);

   function Is_Error (This : Test_Result) return Boolean
   is (This.Result = Error);

   function Expected (This : Test_Result) return String
   is (Ada.Strings.Unbounded.To_String (This.Expected));

   function Found (This : Test_Result) return String
   is (Ada.Strings.Unbounded.To_String (This.Found));

   function Error_Message (This : Test_Result) return String
   is (Ada.Strings.Unbounded.To_String (This.Message));

   type Unit_Test is abstract tagged
      record
         null;
      end record;

   package Unit_Test_Holders is
     new Ada.Containers.Indefinite_Holders (Unit_Test'Class);

   package Test_Result_Holders is
     new Ada.Containers.Indefinite_Holders (Test_Result);

   type Unit_Test_Record is
      record
         Test   : Unit_Test_Holders.Holder;
         Result : Test_Result_Holders.Holder;
      end record;

   package Unit_Test_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Unit_Test_Record);

   type Test_Suite is tagged
      record
         List           : Unit_Test_Lists.List;
         Is_Verbose     : Boolean := False;
         Verbose_Errors : Boolean := False;
      end record;

end WL.Unit;
