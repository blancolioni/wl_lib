with Ada.Numerics.Elementary_Functions;

package body WL.Brownian_Noise is

   ---------
   -- Get --
   ---------

   function Get
     (Noise      : Brownian_Noise_Type'Class;
      Coordinate : WL.Noise.Noise_Vector;
      Octave     : Float)
      return Signed_Unit_Real
   is
      Result : Float := 0.0;
      Temp   : WL.Noise.Noise_Vector := Coordinate;
      Last_Octave : constant Octave_Range :=
                      Octave_Range (Octave);
   begin
      for I in 1 .. Last_Octave loop
         Result := Result + Noise.Get (Temp) * Noise.Exponents (I);
         for J in Temp'Range loop
            Temp (J) := Temp (J) * Noise.Lacunarity;
         end loop;
      end loop;

      declare
         Remainder : constant Float :=
                       Octave - Float'Truncation (Octave);
      begin
         if Remainder > 0.0 then
            Result := Result
              + Remainder * Noise.Get (Temp)
              * Noise.Exponents (Last_Octave + 1);
         end if;
      end;

      return (if Result < -1.0
              then -1.0
              elsif Result > 1.0
              then 1.0
              else Result);

   end Get;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Noise       : in out Brownian_Noise_Type'Class;
      Initiator   : Integer;
      Roughness   : Unit_Real;
      Lacunarity  : Float)
   is
      use Ada.Numerics.Elementary_Functions;
      F : Float := 1.0;
   begin
      WL.Noise.Perlin_Noise (Noise).Reset (Initiator);
      Noise.Roughness := Roughness;
      Noise.Lacunarity := Lacunarity;
      for I in Noise.Exponents'Range loop
         Noise.Exponents (I) :=
           F ** (-Roughness);
         F := F * Lacunarity;
      end loop;

   end Reset;

end WL.Brownian_Noise;
