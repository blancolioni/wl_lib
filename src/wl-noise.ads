package WL.Noise is

   subtype Signed_Unit_Real is Float range -1.0 .. 1.0;
   subtype Unit_Real is Float range 0.0 .. 1.0;

   type Dimension_Range is range 1 .. 4;

   type Noise_Vector is array (Dimension_Range range <>) of Float;

   type Perlin_Noise (Dimension_Count : Dimension_Range) is tagged private;

   procedure Reset (Noise : in out Perlin_Noise'Class;
                    Initiator       : Integer);

   function Get
     (Noise      : Perlin_Noise'Class;
      Coordinate : Noise_Vector)
      return Signed_Unit_Real
     with Pre => Coordinate'First = 1
     and then Coordinate'Last = Noise.Dimension_Count;

private

   type Map_Index_Type is mod 256;

   type Map_Type is array (Map_Index_Type) of Map_Index_Type;

   type Dimension_Index_Buffer is
     array (Dimension_Range range <>) of Map_Index_Type;

   type Dimension_Float_Buffer is
     array (Dimension_Range range <>) of Float;

   type Dimension_Buffer_Access is access Dimension_Float_Buffer;

   type Buffer_Type is array (Map_Index_Type) of Dimension_Buffer_Access;

   type Perlin_Noise (Dimension_Count : Dimension_Range) is tagged
      record
         Map             : Map_Type;
         Buffer          : Buffer_Type;
      end record;

   type Index_Record is
      record
         Index     : Map_Index_Type;
         Remainder : Float;
         Cubic     : Float;
      end record;

   type Index_Record_Array is
     array (Dimension_Range range <>) of Index_Record;

   function Lattice
     (Noise : Perlin_Noise;
      Indices : Dimension_Index_Buffer;
      Remainders : Dimension_Float_Buffer)
      return Float
     with Pre => Indices'First = 1
     and then Remainders'First = 1
     and then Indices'Last = Noise.Dimension_Count
     and then Remainders'Last = Noise.Dimension_Count;

end WL.Noise;
