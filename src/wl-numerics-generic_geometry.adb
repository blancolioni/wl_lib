with Ada.Numerics.Generic_Elementary_Functions;

with WL.Generic_Real_Images;

package body WL.Numerics.Generic_Geometry is

   package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Real);

   package Real_Images is new WL.Generic_Real_Images (Real);

   type Orientation_Type is (Colinear, Clockwise, Anticlockwise);

   function Orientation
     (P, Q, R : Point_Type)
      return Orientation_Type;

   function On_Segment
     (P, Q, R : Point_Type)
      return Boolean;

   -------------------
   -- Angle_Between --
   -------------------

   function Angle_Between
     (Angle_1, Angle_2 : Trigonometry.Angle) return Trigonometry.Angle
   is
      use type Trigonometry.Angle;
   begin
      return abs (Angle_1 - Angle_2);
   end Angle_Between;

   -------------------
   -- Angle_Between --
   -------------------

   function Angle_Between
     (Line_1, Line_2 : Line_Segment) return Trigonometry.Angle
   is
   begin
      return Angle_Between (Point_Angle (Line_1.A, Line_1.B),
                            Point_Angle (Line_2.A, Line_2.B));
   end Angle_Between;

   ------------------------------
   -- Closest_Point_On_Segment --
   ------------------------------

   function Closest_Point_On_Segment
     (Point : Point_Type; Segment : Line_Segment) return Point_Type
   is
      P : constant Point_Type := Point;
      A : constant Point_Type := Segment.A;
      B : constant Point_Type := Segment.B;

      Vector_AP : constant Point_Type :=
        (P.X - A.X, P.Y - A.Y);
      Vector_AB : constant Point_Type :=
        (B.X - A.X, B.Y - A.Y);
      Magnitude_AB : constant Non_Negative_Real :=
        Vector_AB.X ** 2 + Vector_AB.Y ** 2;

      AB_AP_Product : constant Real :=
        Vector_AB.X * Vector_AP.X + Vector_AB.Y * Vector_AP.Y;

      Distance : constant Real :=
        (if Magnitude_AB = 0.0 then 0.0 else AB_AP_Product / Magnitude_AB);

   begin
      if Distance <= 0.0 then
         return A;
      elsif Distance >= 1.0 then
         return B;
      else
         return (A.X + Vector_AB.X * Distance,
                 A.Y + Vector_AB.Y * Distance);
      end if;
   end Closest_Point_On_Segment;

   -----------------
   -- Convex_Hull --
   -----------------

   function Convex_Hull
     (Container : Point_Vectors.Vector)
      return Point_Vectors.Vector
   is
      function Left_Of (Left, Right : Point_Type) return Boolean
      is (Left.X < Right.X);

      package Sorting is
        new Point_Vectors.Generic_Sorting (Left_Of);

      Copy : Point_Vectors.Vector := Container;
   begin
      Sorting.Sort (Copy);

      return Result : Point_Vectors.Vector do
         declare
            Point_On_Hull : Point_Type := Copy.First_Element;
         begin
            loop
               Result.Append (Point_On_Hull);

               declare
                  Endpoint : Point_Type := Copy.First_Element;
               begin
                  for Pt of Copy loop
                     if Endpoint = Point_On_Hull
                       or else Orientation (Point_On_Hull, Endpoint, Pt)
                       = Clockwise
                     then
                        Endpoint := Pt;
                     end if;
                  end loop;

                  exit when Endpoint = Copy.First_Element;

                  Point_On_Hull := Endpoint;
               end;
            end loop;
         end;
      end return;
   end Convex_Hull;

   --------------
   -- Distance --
   --------------

   function Distance
     (P1, P2 : Point_Type)
      return Non_Negative_Real
   is
   begin
      return Elementary_Functions.Sqrt
        ((P1.X - P2.X) ** 2 + (P1.Y - P2.Y) ** 2);
   end Distance;

   -----------
   -- Image --
   -----------

   function Image (Point : Point_Type) return String is
   begin
      return "("
        & Real_Images.Approximate_Image (Point.X)
        & ","
        & Real_Images.Approximate_Image (Point.Y)
        & ")";
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Line : Line_Segment) return String is
   begin
      return Image (Line.A) & "->" & Image (Line.B);
   end Image;

   ------------------
   -- Intersection --
   ------------------

   function Intersection (Line_1, Line_2 : Line_Segment) return Point_Type is
      P1  : constant Point_Type := Line_1.A;
      Q1  : constant Point_Type := Line_1.B;
      P2  : constant Point_Type := Line_2.A;
      Q2  : constant Point_Type := Line_2.B;
      A1  : constant Real := (P1.X * Q1.Y - P1.Y * Q1.X);
      A2  : constant Real := (P2.X * Q2.Y - P2.Y * Q2.X);
      DX1 : constant Real := P1.X - Q1.X;
      DX2 : constant Real := P2.X - Q2.X;
      DY1 : constant Real := P1.Y - Q1.Y;
      DY2 : constant Real := P2.Y - Q2.Y;
      M   : constant Real := (DX1 * DY2 - DY1 * DX2);
      PX  : constant Real := (A1 * DX2 - A2 * DX1) / M;
      PY  : constant Real := (A1 * DY2 - A2 * DY1) / M;
   begin
      return (PX, PY);
   end Intersection;

   ----------------
   -- Intersects --
   ----------------

   function Intersects (Line_1, Line_2 : Line_Segment) return Boolean is
      P1 : constant Point_Type := Line_1.A;
      Q1 : constant Point_Type := Line_1.B;
      P2 : constant Point_Type := Line_2.A;
      Q2 : constant Point_Type := Line_2.B;

      O : constant array (1 .. 4) of Orientation_Type :=
        (Orientation (P1, Q1, P2),
         Orientation (P1, Q1, Q2),
         Orientation (P2, Q2, P1),
         Orientation (P2, Q2, Q1));
   begin
      if P1 = P2 or else P1 = Q2
        or else Q1 = P2 or else Q1 = Q2
      then
         return True;
      end if;

      if O (1) /= O (2) and then O (3) /= O (4) then
         return True;
      end if;

      if O (1) = Colinear and then On_Segment (P1, P2, Q1) then
         return True;
      end if;

      if O (2) = Colinear and then On_Segment (P1, Q2, Q1) then
         return True;
      end if;

      if O (3) = Colinear and then On_Segment (P2, P1, Q2) then
         return True;
      end if;

      if O (4) = Colinear and then On_Segment (P2, Q1, Q2) then
         return True;
      end if;

      return False;
   end Intersects;

   ------------
   -- Length --
   ------------

   function Length (Line : Line_Segment) return Non_Negative_Real is
   begin
      return Distance (Line.A, Line.B);
   end Length;

   ----------------
   -- On_Segment --
   ----------------

   function On_Segment
     (P, Q, R : Point_Type)
      return Boolean
   is
   begin
      return Q.X <= Real'Max (P.X, R.X)
        and then Q.X >= Real'Min (P.X, R.X)
        and then Q.Y <= Real'Max (P.Y, R.Y)
        and then Q.Y >= Real'Min (P.Y, R.Y);
   end On_Segment;

   -----------------
   -- Orientation --
   -----------------

   function Orientation
     (P, Q, R : Point_Type)
      return Orientation_Type
   is
      Turn : constant Real :=
        (Q.Y - P.Y) * (R.X - Q.X) -
        (Q.X - P.X) * (R.Y - Q.Y);
   begin
      return (if Turn = 0.0 then Colinear
              elsif Turn > 0.0 then Clockwise
              else Anticlockwise);
   end Orientation;

   --------------
   -- Parallel --
   --------------

   function Parallel
     (Line_1, Line_2 : Line_Segment)
      return Boolean
   is
      DX_1 : constant Real := Line_1.B.X - Line_1.A.X;
      DX_2 : constant Real := Line_2.B.X - Line_2.A.X;
      DY_1 : constant Real := Line_1.B.Y - Line_1.A.Y;
      DY_2 : constant Real := Line_2.B.Y - Line_2.A.Y;
   begin
      if (DX_1 = 0.0 and then DX_2 = 0.0)
        or else (DY_1 = 0.0 and then DY_2 = 0.0)
      then
         return True;
      else
         return abs (DY_1 * DX_2 - DY_2 * DX_1) < 0.001;
      end if;
   end Parallel;

   -----------------
   -- Point_Angle --
   -----------------

   function Point_Angle (From, To : Point_Type) return Trigonometry.Angle is
      X : constant Real := To.X - From.X;
      Y : constant Real := To.Y - From.Y;
   begin
      if X = 0.0 and then Y = 0.0 then
         return Trigonometry.From_Degrees (0.0);
      else
         return Trigonometry.Arctan (Y, X);
      end if;
   end Point_Angle;

   ----------------------
   -- Point_On_Segment --
   ----------------------

   function Point_On_Segment
     (Point : Point_Type; Segment : Line_Segment) return Boolean
   is
   begin
      return Orientation (Segment.A, Point, Segment.B) = Colinear
        and then On_Segment (Segment.A, Point, Segment.B);
   end Point_On_Segment;

   ------------
   -- Rotate --
   ------------

   function Rotate
     (Point : Point_Type;
      About : Point_Type;
      Angle : Trigonometry.Angle)
      return Point_Type
   is
      use Trigonometry;
      Old_DX : constant Real := Point.X - About.X;
      Old_DY : constant Real := Point.Y - About.Y;
      New_DX : constant Real :=
        Old_DX * Cos (Angle) - Old_DY * Sin (Angle);
      New_DY : constant Real :=
        Old_DX * Sin (Angle) + Old_DY * Cos (Angle);

   begin
      return (About.X + New_DX, About.Y + New_DY);
   end Rotate;

end WL.Numerics.Generic_Geometry;
