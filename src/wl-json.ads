private with Ada.Containers.Indefinite_Vectors;
private with WL.String_Maps;

package WL.Json is

   type Json_Value is abstract tagged private;

   function Null_Value return Json_Value'Class;
   function Boolean_Value (Bool : Boolean) return Json_Value'Class;
   function String_Value (Text : String) return Json_Value'Class;
   function Integer_Value (Int : Integer) return Json_Value'Class;
   function Float_Value (F : Float) return Json_Value'Class;

   function Is_Null (Value : Json_Value'Class) return Boolean;

   function Serialize
     (Value : Json_Value)
      return String
      is abstract;

   function Get_Property
     (Value : Json_Value;
      Name  : String)
      return Json_Value'Class;

   function Get_Property
     (Value : Json_Value'Class;
      Name  : String)
      return String;

   function Image
     (Value : Json_Value)
      return String;

   procedure Save
     (Value : Json_Value'Class;
      Path  : String);
   --  Write an indented representaion of Value to
   --  the file indicated by Path.

   type Json_Object is new Json_Value with private;

   overriding function Serialize
     (Value : Json_Object)
      return String;

   overriding function Get_Property
     (Object : Json_Object;
      Name   : String)
      return Json_Value'Class;

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Json_Value'Class);

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : String);

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Integer);

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Float);

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Boolean);

   procedure Copy
     (To   : in out Json_Object'Class;
      From : Json_Object'Class);

   type Json_Array is new Json_Value with private;

   overriding function Serialize
     (Value : Json_Array)
      return String;

   procedure Append
     (To     : in out Json_Array'Class;
      Value  : Json_Value'Class);

private

   type Json_Value is abstract tagged null record;

   function Image
     (Value : Json_Value)
      return String
   is (Json_Value'Class (Value).Serialize);

   package Json_Value_Maps is
     new WL.String_Maps (Json_Value'Class);

   package Json_Value_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Json_Value'Class);

   type Json_Object is new Json_Value with
      record
         Properties : Json_Value_Maps.Map;
      end record;

   overriding function Image (Object : Json_Object) return String
   is ("[Object]");

   type Json_Array is new Json_Value with
      record
         Vector : Json_Value_Vectors.Vector;
      end record;

   overriding function Image (Item : Json_Array) return String
   is ("[Array]");

   type Atomic_Json_Value is abstract new Json_Value with null record;

end WL.Json;
