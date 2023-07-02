with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;

package body WL.Noise is

   procedure Normalise (Dimension : in out Dimension_Float_Buffer);

   function Interpolate
     (A, B : Float;
      X    : Unit_Real)
      return Float
   is (A + X * (B - A));

   ---------
   -- Get --
   ---------

   function Get
     (Noise      : Perlin_Noise'Class;
      Coordinate : Noise_Vector)
      return Signed_Unit_Real
   is

      Indices : Index_Record_Array (1 .. Noise.Dimension_Count);

      Result  : Float;

   begin
      for I in Indices'Range loop
         declare
            Float_Index : constant Float :=
                            Float'Truncation (Coordinate (I));
            Index       : constant Map_Index_Type :=
                            Map_Index_Type
                              (Integer (Float_Index)
                               mod Map_Index_Type'Modulus);
            Remainder   : constant Float :=
                            Coordinate (I) - Float_Index;
            Cubic       : constant Float :=
                            Remainder ** 2 * (3.0 - 2.0 * Remainder);
         begin
            Indices (I) := (Index, Remainder, Cubic);
         end;
      end loop;

      case Noise.Dimension_Count is
         when 1 =>
            Result := Interpolate
              (Noise.Lattice
                 ((1 => Indices (1).Index), (1 => Indices (1).Remainder)),
               Noise.Lattice
                 ((1 => Indices (1).Index + 1),
                  (1 => Indices (1).Remainder - 1.0)),
               Indices (1).Cubic);
         when 2 =>
            declare
               Index_1     : constant Map_Index_Type := Indices (1).Index;
               Index_2     : constant Map_Index_Type := Indices (2).Index;
               Remainder_1 : constant Float := Indices (1).Remainder;
               Remainder_2 : constant Float := Indices (2).Remainder;
               Cubic_1     : constant Float := Indices (1).Cubic;
               Cubic_2     : constant Float := Indices (2).Cubic;
               A           : constant Float :=
                               Interpolate
                                 (Noise.Lattice
                                    ((Index_1, Index_2),
                                     (Remainder_1, Remainder_2)),
                                  Noise.Lattice
                                    ((Index_1 + 1, Index_2),
                                     (Remainder_1 - 1.0, Remainder_2)),
                                  Cubic_1);
               B           : constant Float :=
                               Interpolate
                                 (Noise.Lattice
                                    ((Index_1, Index_2 + 1),
                                     (Remainder_1, Remainder_2 - 1.0)),
                                  Noise.Lattice
                                    ((Index_1 + 1, Index_2 + 1),
                                     (Remainder_1 - 1.0, Remainder_2 - 1.0)),
                                  Cubic_1);
            begin
               Result := Interpolate (A, B, Cubic_2);
            end;

         when 3 =>
            declare
               Index_1 : constant Map_Index_Type := Indices (1).Index;
               Index_2 : constant Map_Index_Type := Indices (2).Index;
               Index_3 : constant Map_Index_Type := Indices (3).Index;
               Rem_1   : constant Float := Indices (1).Remainder;
               Rem_2   : constant Float := Indices (2).Remainder;
               Rem_3   : constant Float := Indices (3).Remainder;
               Cubic_1     : constant Float := Indices (1).Cubic;
               Cubic_2     : constant Float := Indices (2).Cubic;
               Cubic_3     : constant Float := Indices (3).Cubic;
               A_1_1       : constant Float :=
                           Noise.Lattice
                             ((Index_1, Index_2, Index_3),
                              (Rem_1, Rem_2, Rem_3));
               A_1_2   : constant Float :=
                           Noise.Lattice
                             ((Index_1 + 1, Index_2, Index_3),
                              (Rem_1 - 1.0, Rem_2, Rem_3));
               A_2_1   : constant Float :=
                           Noise.Lattice
                             ((Index_1, Index_2 + 1, Index_3),
                              (Rem_1, Rem_2 - 1.0, Rem_3));
               A_2_2   : constant Float :=
                           Noise.Lattice
                             ((Index_1 + 1, Index_2 + 1, Index_3),
                              (Rem_1 - 1.0, Rem_2 - 1.0, Rem_3));
               B_1_1   : constant Float :=
                           Noise.Lattice
                             ((Index_1, Index_2, Index_3 + 1),
                              (Rem_1, Rem_2, Rem_3 - 1.0));
               B_1_2   : constant Float :=
                           Noise.Lattice
                             ((Index_1 + 1, Index_2, Index_3 + 1),
                              (Rem_1 - 1.0, Rem_2, Rem_3 - 1.0));
               B_2_1   : constant Float :=
                           Noise.Lattice
                             ((Index_1, Index_2 + 1, Index_3 + 1),
                              (Rem_1, Rem_2 - 1.0, Rem_3 - 1.0));
               B_2_2   : constant Float :=
                           Noise.Lattice
                             ((Index_1 + 1, Index_2 + 1, Index_3 + 1),
                              (Rem_1 - 1.0, Rem_2 - 1.0, Rem_3 - 1.0));
               A_1     : constant Float :=
                           Interpolate (A_1_1, A_1_2, Cubic_1);
               A_2     : constant Float :=
                           Interpolate (A_2_1, A_2_2, Cubic_1);
               B_1     : constant Float :=
                           Interpolate (B_1_1, B_1_2, Cubic_1);
               B_2     : constant Float :=
                           Interpolate (B_2_1, B_2_2, Cubic_1);
               A       : constant Float :=
                           Interpolate (A_1, A_2, Cubic_2);
               B       : constant Float :=
                           Interpolate (B_1, B_2, Cubic_2);
            begin
               Result := Interpolate (A, B, Cubic_3);
            end;
         when others =>
            Result := 0.0;
      end case;

      return (if Result < -1.0
              then -1.0
              elsif Result > 1.0
              then 1.0
              else Result);
   end Get;

   -------------
   -- Lattice --
   -------------

   function Lattice
     (Noise      : Perlin_Noise;
      Indices    : Dimension_Index_Buffer;
      Remainders : Dimension_Float_Buffer)
      return Float
   is
      Index : Map_Index_Type := 0;
      Result : Float := 0.0;
   begin
      for I in Indices'Range loop
         Index := Noise.Map (Index + Indices (I));
      end loop;

      for I in Remainders'Range loop
         Result := Result + Noise.Buffer (Index) (I) * Remainders (I);
      end loop;

      return Result;
   end Lattice;

   ---------------
   -- Normalise --
   ---------------

   procedure Normalise (Dimension : in out Dimension_Float_Buffer) is
      use Ada.Numerics.Elementary_Functions;
      Norm : Float := 0.0;
   begin
      for X of Dimension loop
         Norm := Norm + X ** 2;
      end loop;
      Norm := 1.0 / Sqrt (Norm);
      for I in Dimension'Range loop
         Dimension (I) := Dimension (I) * Norm;
      end loop;

   end Normalise;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Noise           : in out Perlin_Noise'Class;
      Initiator       : Integer)
   is
      use Ada.Numerics.Float_Random;
      Gen : Generator;
   begin
      Reset (Gen, Initiator);

      for Index in Noise.Map'Range loop
         Noise.Map (Index) := Index;
         Noise.Buffer (Index) :=
           new Dimension_Float_Buffer (1 .. Noise.Dimension_Count);

         for Dimension in Noise.Buffer (Index)'Range loop
            Noise.Buffer (Index) (Dimension) :=
              Random (Gen) - 0.5;
         end loop;

         Normalise (Noise.Buffer (Index).all);
      end loop;

      for Index in reverse Noise.Map'Range loop
         declare
            Target_Index : constant Map_Index_Type :=
                             Map_Index_Type (Natural (Random (Gen) * 65535.0)
                                             / 256);
            Temp         : constant Map_Index_Type :=
                             Noise.Map (Index);
         begin
            Noise.Map (Index) := Noise.Map (Target_Index);
            Noise.Map (Target_Index) := Temp;
         end;
      end loop;

   end Reset;

end WL.Noise;
