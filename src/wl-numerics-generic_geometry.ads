with Ada.Containers.Vectors;

with WL.Numerics.Generic_Trigonometry;

generic
   type Real is digits <>;
package WL.Numerics.Generic_Geometry is

   package Trigonometry is
     new WL.Numerics.Generic_Trigonometry (Real);

   subtype Unit_Real is Real range 0.0 .. 1.0;
   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   subtype Non_Negative_Real is Real range 0.0 .. Real'Last;

   type Point_Type is
      record
         X, Y : Real;
      end record;

   function Distance (P1, P2 : Point_Type) return Non_Negative_Real;

   function Image (Point : Point_Type) return String;

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Point_Type);

   function Convex_Hull
     (Container : Point_Vectors.Vector)
      return Point_Vectors.Vector;

   type Line_Segment is
      record
         A, B : Point_Type;
      end record;

   function Image (Line : Line_Segment) return String;
   function Length (Line : Line_Segment) return Non_Negative_Real;

   function Intersects
     (Line_1, Line_2 : Line_Segment)
      return Boolean;

   function Parallel
     (Line_1, Line_2 : Line_Segment)
      return Boolean;

   function Point_On_Segment
     (Point : Point_Type;
      Segment : Line_Segment)
      return Boolean;

   function Closest_Point_On_Segment
     (Point   : Point_Type;
      Segment : Line_Segment)
      return Point_Type;

   function Point_Angle
     (From, To : Point_Type)
      return Trigonometry.Angle;

   function Angle_Between
     (Angle_1, Angle_2 : Trigonometry.Angle)
      return Trigonometry.Angle;

   function Angle_Between
     (Line_1, Line_2 : Line_Segment)
      return Trigonometry.Angle;

   function Intersection
     (Line_1, Line_2 : Line_Segment)
      return Point_Type
     with Pre => Intersects (Line_1, Line_2);

   function Rotate
     (Point : Point_Type;
      About : Point_Type;
      Angle : Trigonometry.Angle)
      return Point_Type;

   function Rotate
     (Segment : Line_Segment;
      About   : Point_Type;
      Angle   : Trigonometry.Angle)
      return Line_Segment
   is (Rotate (Segment.A, About, Angle),
       Rotate (Segment.B, About, Angle));

end WL.Numerics.Generic_Geometry;
