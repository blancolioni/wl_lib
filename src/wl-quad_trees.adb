with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

with WL.Processes;

package body WL.Quad_Trees is

   Trace_Quad_Tree : constant Boolean := False;

   type Quad is (NW, NE, SW, SE);

   pragma Unreferenced (SE);

   type Quad_Node_Children is array (Quad) of Quad_Tree_Node_Access;

   type Area is
      record
         X1, Y1, X2, Y2 : Natural;
      end record;

   type Quad_Tree_Node (Leaf_Node : Boolean) is
      record
         case Leaf_Node is
            when False =>
               Child : Quad_Node_Children;
            when True =>
               Contents : Property_Type;
         end case;
      end record;

   Read_Process : Processes.Process_Type;

   function Get_Quad (Current   : Area;
                      Quad_Name : Quad)
                      return Area;

   function Get_Quad (Bound   : Area;
                      X, Y    : Natural)
                     return Quad;

   procedure Read_Quad_Tree_Node
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Quad_Tree_Node);

   procedure Write_Quad_Tree_Node
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Quad_Tree_Node);

   for Quad_Tree_Node'Read use Read_Quad_Tree_Node;
   for Quad_Tree_Node'Write use Write_Quad_Tree_Node;

--     package List_Of_Quad_Tree_Nodes is
--       new Ada.Containers.Doubly_Linked_Lists (Quad_Tree_Node_Access, "=");

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree (Width, Height   : Natural;
                         Get_XY_Prop   : not null access
                         function (X, Y : Natural) return Property_Type)
                        return Quad_Tree
   is

      Top_Tree : Quad_Tree_Node_Access;

      type Intermediate is
         record
            Bound     : Area;
            Parent    : Quad_Tree_Node_Access;
            Quad_Name : Quad;
         end record;

      package List_Of_Intermediates is
         new Ada.Containers.Doubly_Linked_Lists (Intermediate);

      function Single_Value (Bound : Area) return Boolean;

      ------------------
      -- Single_Value --
      ------------------

      function Single_Value (Bound : Area) return Boolean is
      begin
         if Bound.X1 = Bound.X2 and then
           Bound.Y1 = Bound.Y2
         then
            return True;
         elsif Bound.Y2 > Bound.Y1 and then
           (Bound.X2 - Bound.X1) > 10_000 / (Bound.Y2 - Bound.Y1)
         then
            return False;
         end if;

         declare
            Value : constant Property_Type :=
              Get_XY_Prop (Bound.X1, Bound.Y1);
         begin
            for X in Bound.X1 .. Bound.X2 loop
               for Y in Bound.Y1 .. Bound.Y2 loop
                  if Get_XY_Prop (X, Y) /= Value then
                     return False;
                  end if;
               end loop;
            end loop;
         end;
         return True;

      exception
         when others =>
            Ada.Text_IO.Put_Line ("Missing value at" &
                                    Bound.X1'Img & Bound.Y1'Img);
            return False;
      end Single_Value;

      Stack : List_Of_Intermediates.List;
      Count : Natural := 0;
   begin

      Stack.Append (((0, 0, Width - 1, Height - 1), null, NW));

      while not Stack.Is_Empty loop
         declare
            Item : constant Intermediate := Stack.First_Element;
            Tree : Quad_Tree_Node_Access;
         begin

            Count := Count + 1;
            if Count mod 100_000 = 0 then
               Ada.Text_IO.Put (".");
               Ada.Text_IO.Flush;
            end if;

            Stack.Delete_First;

            if not Single_Value (Item.Bound) then

               Tree := new Quad_Tree_Node'(False, (others => null));

               for Q in Quad loop
                  declare
                     Quad_Bound : constant Area := Get_Quad (Item.Bound, Q);
                  begin
                     if Quad_Bound.X1 <= Quad_Bound.X2 and then
                       Quad_Bound.Y1 <= Quad_Bound.Y2
                     then
                        Stack.Append ((Quad_Bound, Tree, Q));
                     end if;
                  end;
               end loop;

            else

               Tree := new Quad_Tree_Node'(True, Get_XY_Prop (Item.Bound.X1,
                                                              Item.Bound.Y1));

            end if;

            if Item.Parent /= null then
               Item.Parent.Child (Item.Quad_Name) := Tree;
            else
               Top_Tree := Tree;
            end if;
         end;
      end loop;
      Ada.Text_IO.New_Line;

      return (Width, Height, Top_Tree);

   end Create_Tree;

   --------------
   -- Get_Quad --
   --------------

   function Get_Quad (Current   : Area;
                      Quad_Name : Quad)
                     return Area
   is
      Result : Area := Current;
      Mid_X  : constant Natural := (Current.X1 + Current.X2) / 2;
      Mid_Y  : constant Natural := (Current.Y1 + Current.Y2) / 2;
   begin
      if Quad_Name = NW or else Quad_Name = NE then
         Result.Y2 := Mid_Y;
      else
         Result.Y1 := Mid_Y + 1;
      end if;
      if Quad_Name = NW or else Quad_Name = SW then
         Result.X2 := Mid_X;
      else
         Result.X1 := Mid_X + 1;
      end if;
      return Result;
   end Get_Quad;

   --------------
   -- Get_Quad --
   --------------

   function Get_Quad (Bound   : Area;
                      X, Y    : Natural)
                     return Quad
   is
      Mid_X : constant Natural := (Bound.X1 + Bound.X2) / 2;
      Mid_Y : constant Natural := (Bound.Y1 + Bound.Y2) / 2;
      East  : constant Boolean := X > Mid_X;
      South : constant Boolean := Y > Mid_Y;
   begin
      return Quad'Val (Boolean'Pos (South) * 2 +
                         Boolean'Pos (East));
   end Get_Quad;

   ------------
   -- Height --
   ------------

   function Height (Tree : Quad_Tree) return Natural is
   begin
      return Tree.Height;
   end Height;

   --------------
   -- Property --
   --------------

   function Property (Tree : Quad_Tree;
                      X, Y : Natural)
                     return Property_Type
   is
      function Get (Node  : Quad_Tree_Node_Access;
                    Bound : Area)
                  return Property_Type;

      ---------
      -- Get --
      ---------

      function Get (Node  : Quad_Tree_Node_Access;
                    Bound : Area)
                   return Property_Type
      is
      begin
         if Node = null then
            return No_Property;
         elsif Node.Leaf_Node then
            return Node.Contents;
         else
            declare
               New_Quad  : constant Quad := Get_Quad (Bound, X, Y);
               New_Bound : constant Area := Get_Quad (Bound, New_Quad);
            begin
               return Get (Node.Child (New_Quad), New_Bound);
            end;
         end if;
      end Get;

   begin
      return Get (Tree.Top, (0, 0, Tree.Width - 1, Tree.Height - 1));
   end Property;

   ----------
   -- Read --
   ----------

   function Read (Path : String) return Quad_Tree is
      use Ada.Streams.Stream_IO;
      File   : File_Type;
      Result : Quad_Tree;
   begin
      Open (File, In_File, Path);
      Quad_Tree'Read (Stream (File), Result);
      Close (File);
      return Result;
   end Read;

   ---------------------
   -- Read_Properties --
   ---------------------

   procedure Read_Properties (Tree             : Quad_Tree;
                              Result           : out Rectangle)
   is

      Depth : Positive := 1;

      procedure Get (Node  : Quad_Tree_Node_Access;
                     Bound : Area);

      ---------
      -- Get --
      ---------

      procedure Get (Node  : Quad_Tree_Node_Access;
                     Bound : Area)
      is
      begin
         if Trace_Quad_Tree then
            Ada.Text_IO.Set_Col
              (Ada.Text_IO.Positive_Count (Depth * 2));
            Ada.Text_IO.Put_Line
              ("Get: " &
               Integer'Image (Bound.X1) &
               Integer'Image (Bound.Y1) &
               Integer'Image (Bound.X2) &
               Integer'Image (Bound.Y2));
         end if;

         if Node = null then
            return;
         elsif Node.Leaf_Node then
            declare
               X1   : Natural := Bound.X1;
               Y1   : Natural := Bound.Y1;
               X2   : Natural := Bound.X2;
               Y2   : Natural := Bound.Y2;
            begin
               if X1 < Result'First (1) then
                  X1 := Result'First (1);
               end if;
               if X2 > Result'Last (1) then
                  X2 := Result'Last (1);
               end if;
               if Y1 < Result'First (2) then
                  Y1 := Result'First (2);
               end if;
               if Y2 > Result'Last (2) then
                  Y2 := Result'Last (2);
               end if;

               if Trace_Quad_Tree then
                  Ada.Text_IO.Set_Col
                    (Ada.Text_IO.Positive_Count (Depth * 2));
                  Ada.Text_IO.Put_Line
                    ("    Property "  &
                     Integer'Image (X1) &
                     Integer'Image (Y1) &
                     Integer'Image (X2) &
                     Integer'Image (Y2) &
                     To_String (Node.Contents));
               end if;

               for Y in Y1 .. Y2 loop
                  for X in X1 .. X2 loop
                     Result (X, Y) := Node.Contents;
                  end loop;
               end loop;
            end;
         else
            Depth := Depth + 1;
            for Child in Node.Child'Range loop
               declare
                  A : constant Area := Get_Quad (Bound, Child);
               begin
                  if ((A.X1 in Result'Range (1)
                       or else A.X2 in Result'Range (1))
                      and then
                        (A.Y1 in Result'Range (2)
                         or else A.Y2 in Result'Range (2)))
                    or else
                      ((Result'First (1) in A.X1 .. A.X2
                        or else Result'Last (1) in A.X1 .. A.X2)
                       and then
                         (Result'First (2) in A.Y1 .. A.Y2
                          or else Result'Last (2) in A.Y1  .. A.Y2))
                  then
                     Get (Node.Child (Child), A);
                  end if;
               end;
            end loop;
            Depth := Depth - 1;
         end if;
      end Get;

   begin
      if Trace_Quad_Tree then
         Ada.Text_IO.Put_Line
           ("Reading properties:" &
            Integer'Image (Result'First (1)) &
            Integer'Image (Result'Last (1)) &
            Integer'Image (Result'First (2)) &
            Integer'Image (Result'Last (2)));
      end if;

      Get (Tree.Top, (0, 0, Tree.Width - 1, Tree.Height - 1));
   end Read_Properties;

   --------------------
   -- Read_Quad_Tree --
   --------------------

   procedure Read_Quad_Tree
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Quad_Tree)
   is
--      List : List_Of_Quad_Tree_Nodes.List;
   begin

      Read_Process.Start_Spinner ("reading quad tree", 1_000_000);

      Natural'Read (Stream, Item.Width);
      Natural'Read (Stream, Item.Height);
      Item.Top := new Quad_Tree_Node'(Quad_Tree_Node'Input (Stream));

      Read_Process.Finish;

   end Read_Quad_Tree;

--        List.Append (new Quad_Tree_Node (Boolean'Input (Stream)));
--
--        while not List.Is_Empty loop
--
--           declare
--              Front : constant Quad_Tree_Node_Access := List.First_Element;
--           begin
--              List.Delete_First;
--
--              if Front.Leaf_Node then
--                 Front.Contents := From_String (String'Input (Stream));
--              else
--                 for Q in Front.Child'Range loop
--                    if Boolean'Input (Stream) then
--                       Front.Child (Q) :=
--                         new Quad_Tree_Node (Boolean'Input (Stream));
--                       List.Append (Front.Child (Q));
--                    end if;
--                 end loop;
--              end if;
--           end;
--
--        end loop;
--
--     end Read_Quad_Tree;

   -------------------------
   -- Read_Quad_Tree_Node --
   -------------------------

   procedure Read_Quad_Tree_Node
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Quad_Tree_Node)
   is
   begin
      Read_Process.Tick;

      if Item.Leaf_Node then
         declare
            Id : constant String := String'Input (Stream);
         begin
            Item.Contents := From_String (Id);
         end;
      else
         for Q in Item.Child'Range loop
            declare
               Exists : Boolean;
            begin
               Boolean'Read (Stream, Exists);
               if Exists then
                  Item.Child (Q) :=
                    new Quad_Tree_Node'(Quad_Tree_Node'Input (Stream));
               end if;
            end;
         end loop;
      end if;
   end Read_Quad_Tree_Node;

   -----------
   -- Width --
   -----------

   function Width (Tree : Quad_Tree) return Natural is
   begin
      return Tree.Width;
   end Width;

   -----------
   -- Write --
   -----------

   procedure Write (Tree   : Quad_Tree;
                    Path   : String)
   is
      use Ada.Streams.Stream_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      Quad_Tree'Write (Stream (File), Tree);
      Close (File);
   end Write;

   ---------------------
   -- Write_Quad_Tree --
   ---------------------

   procedure Write_Quad_Tree
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Quad_Tree)
   is
      --  List : List_Of_Quad_Tree_Nodes.List;
   begin
      Natural'Write (Stream, Item.Width);
      Natural'Write (Stream, Item.Height);
      Quad_Tree_Node'Output (Stream, Item.Top.all);

--        List.Append (Item.Top);
--
--        while not List.Is_Empty loop
--
--           declare
--              Front : constant Quad_Tree_Node_Access := List.First_Element;
--           begin
--              List.Delete_First;
--
--              Boolean'Write (Stream, Front.Leaf_Node);
--
--              if Front.Leaf_Node then
--                 String'Output (Stream, To_String (Front.Contents));
--              else
--                 for Q in Front.Child'Range loop
--                    Boolean'Write (Stream, Front.Child (Q) /= null);
--                    if Front.Child (Q) /= null then
--                       List.Append (Front.Child (Q));
--                    end if;
--                 end loop;
--              end if;
--           end;
--        end loop;

   end Write_Quad_Tree;

   --------------------------
   -- Write_Quad_Tree_Node --
   --------------------------

   procedure Write_Quad_Tree_Node
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Quad_Tree_Node)
   is
   begin
      if Item.Leaf_Node then
         String'Output (Stream, To_String (Item.Contents));
      else
         for Q in Item.Child'Range loop
            if Item.Child (Q) /= null then
               Boolean'Write (Stream, True);
               Quad_Tree_Node'Output (Stream, Item.Child (Q).all);
            else
               Boolean'Write (Stream, False);
            end if;
         end loop;
      end if;
   end Write_Quad_Tree_Node;

end WL.Quad_Trees;
