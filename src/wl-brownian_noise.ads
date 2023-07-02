with WL.Noise;                         use WL.Noise;

package WL.Brownian_Noise is

   type Octave_Range is range 1 .. 128;

   type Brownian_Noise_Type
     (Dimension_Count : WL.Noise.Dimension_Range)
   is new WL.Noise.Perlin_Noise with private;

   procedure Reset
     (Noise       : in out Brownian_Noise_Type'Class;
      Initiator   : Integer;
      Roughness   : Unit_Real;
      Lacunarity  : Float);

   function Get
     (Noise      : Brownian_Noise_Type'Class;
      Coordinate : WL.Noise.Noise_Vector;
      Octave     : Float)
      return Signed_Unit_Real
     with Pre => WL.Noise."=" (Coordinate'First, 1)
     and then WL.Noise."=" (Coordinate'Last, Noise.Dimension_Count);

private

   type Exponent_Array is array (Octave_Range) of Float;

   type Brownian_Noise_Type
     (Dimension_Count : WL.Noise.Dimension_Range)
   is new WL.Noise.Perlin_Noise (Dimension_Count) with
      record
         Roughness  : Unit_Real;
         Lacunarity : Float;
         Exponents  : Exponent_Array;
      end record;

end WL.Brownian_Noise;
