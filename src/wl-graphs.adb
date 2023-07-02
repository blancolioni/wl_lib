--  with Ada.Text_IO;
with WL.Heaps;

package body WL.Graphs is

   ------------
   -- Append --
   ------------

   procedure Append
     (Container : in out Graph;
      Vertex    : Vertex_Type)
   is
   begin
      Container.Vertices.Append
        (Vertex_Info'
           (Vertex => Vertex,
            Index  => Container.Vertices.Last_Index + 1,
            Edges  => Edge_Lists.Empty_List));
      Container.Vs.Append (Vertex);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Sub   : in out Sub_Graph;
      Index : Index_Type)
   is
   begin
      Sub.Vertex_List.Append (Index);
      Sub.Vertex_Flags.Replace_Element (Index, True);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Collection : in out Sub_Graph_Collection;
      Sub        : Sub_Graph)
   is
   begin
      Collection.Vector.Append (Sub);
   end Append;

   ------------------------
   -- Breadth_First_Scan --
   ------------------------

   procedure Breadth_First_Scan
     (Container : Graph;
      Start     : Index_Type;
      Process   : not null access
        procedure (Path_To : Path))
   is
      package Frontier_Queues is
        new WL.Heaps
          (Key_Type     => Cost_Type,
           Element_Type => Index_Type,
           "<"          => ">");

      Sentinel : constant Extended_Index := Extended_Index'First;

      type Visited_Info is
         record
            Visited     : Boolean := False;
            Came_From   : Extended_Index := Sentinel;
            Cost_So_Far : Cost_Type := 0.0;
         end record;

      Visited  : array (1 .. Container.Vertices.Last_Index) of Visited_Info;
      Frontier : Frontier_Queues.Heap;

   begin

      Frontier.Insert (0.0, Start);
      Visited (Start).Visited := True;

      while not Frontier.Is_Empty loop
         declare
            Current      : constant Index_Type :=
                             Frontier.First_Element;
            Current_Cost : constant Cost_Type :=
                             Visited (Current).Cost_So_Far;
         begin
            Frontier.Delete_First;

            if Current /= Start then
               declare
                  P  : Path;
                  It : Extended_Index := Current;
               begin
                  while Visited (It).Came_From /= Sentinel loop

                     if P.List.Is_Empty then
                        P.List.Append (It);
                     else
                        P.List.Insert (P.List.First, It);
                     end if;

                     It := Visited (It).Came_From;
                  end loop;

                  P.Cost := Current_Cost;

                  Process (P);
               end;
            end if;

            for Edge of Container.Vertices.Element (Current).Edges loop
               declare
                  New_Cost : constant Cost_Type :=
                               Current_Cost + Edge.Cost;
                  Info     : Visited_Info renames Visited (Edge.To);
               begin
                  if not Info.Visited then
                     Frontier.Insert
                       (New_Cost, Edge.To);
                     Info.Visited := True;
                     Info.Cost_So_Far := New_Cost;
                     Info.Came_From := Current;
                  end if;
               end;
            end loop;
         end;
      end loop;

   end Breadth_First_Scan;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   function Breadth_First_Search
     (Container : Graph;
      Start     : Vertex_Type;
      Test      : not null access
        function (Vertex : Vertex_Type) return Boolean)
      return Vertex_Type
   is
      package Queue_Of_Partials is
        new Ada.Containers.Doubly_Linked_Lists (Index_Type);
      Queue : Queue_Of_Partials.List;
      Tested : Queue_Of_Partials.List;
   begin
      Queue.Append (Index_Of (Start));

      while not Queue.Is_Empty loop
         declare
            Ix   : constant Index_Type := Queue.First_Element;
         begin
            Queue.Delete_First;
            if Test (Container.Vs (Ix)) then
               return Container.Vs (Ix);
            elsif not Tested.Contains (Ix) then
               Tested.Append (Ix);
               for Edge of Container.Vertices.Element (Ix).Edges loop
                  Queue.Append (Edge.To);
               end loop;
            end if;
         end;
      end loop;

      return Start;

   end Breadth_First_Search;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   procedure Breadth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max_Steps : Count_Type;
      Result    : out Sub_Graph)
   is
      type Partial is
         record
            Index : Index_Type;
            Steps : Count_Type;
         end record;

      package Queue_Of_Partials is
         new Ada.Containers.Doubly_Linked_Lists (Partial);
      Queue : Queue_Of_Partials.List;
   begin
      Container.Create (Result);
      Queue.Append ((Start, 0));
      while not Queue.Is_Empty loop
         declare
            use type Ada.Containers.Count_Type;
            P     : constant Partial := Queue.First_Element;
            Ix    : constant Index_Type := P.Index;
            Steps : constant Count_Type := P.Steps;
         begin
            Queue.Delete_First;
            if not Contains (Result, Ix) then
               Append (Result, Ix);
               if Steps < Max_Steps then
                  for Edge of Container.Vertices.Element (Ix).Edges loop
                     Queue.Append ((Edge.To, Steps + 1));
                  end loop;
               end if;
            end if;
         end;
      end loop;

   end Breadth_First_Search;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   procedure Breadth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max       : Cost_Type;
      Result    : out Sub_Graph)
   is
      type Partial is
         record
            Index : Index_Type;
            Cost  : Cost_Type;
         end record;

      package Queue_Of_Partials is
         new Ada.Containers.Doubly_Linked_Lists (Partial);
      Queue : Queue_Of_Partials.List;
   begin
      Container.Create (Result);
      Queue.Append ((Start, 0.0));
      while not Queue.Is_Empty loop
         declare
            P    : constant Partial := Queue.First_Element;
            Ix   : constant Index_Type := P.Index;
            Cost : constant Cost_Type := P.Cost;
         begin
            Queue.Delete_First;
            if not Contains (Result, Ix) then
               Append (Result, Ix);
               for Edge of Container.Vertices.Element (Ix).Edges loop
                  declare
                     New_Cost : constant Cost_Type := Cost + Edge.Cost;
                  begin
                     if New_Cost <= Max then
                        Queue.Append ((Edge.To, New_Cost));
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

   end Breadth_First_Search;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   procedure Breadth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Test      : not null access
        function (Vertex : Vertex_Type) return Boolean;
      Result    : out Sub_Graph)
   is
      package Queue_Of_Indices is
        new Ada.Containers.Doubly_Linked_Lists (Index_Type);
      Queue : Queue_Of_Indices.List;
   begin
      Container.Create (Result);

      if not Test (Container.Vertex (Start)) then
         return;
      end if;

      Queue.Append (Start);

      while not Queue.Is_Empty loop
         declare
            Ix   : constant Index_Type := Queue.First_Element;
         begin
            Queue.Delete_First;
            if not Contains (Result, Ix) then
               Append (Result, Ix);
               for Edge of Container.Vertices.Element (Ix).Edges loop
                  if Test (Container.Vertex (Edge.To)) then
                     Queue.Append (Edge.To);
                  end if;
               end loop;
            end if;
         end;
      end loop;

   end Breadth_First_Search;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Container : in out Graph'Class;
      From, To  :        Index_Type;
      Cost      :        Cost_Type := Default_Cost)
   is
   begin
      Container.Vertices (From).Edges.Append ((To, Cost));
   end Connect;

   ---------------
   -- Connected --
   ---------------

   function Connected
     (Container    : Graph;
      From, To     : Index_Type)
      return Boolean
   is
   begin
      for Edge of Container.Vertices.Element (From).Edges loop
         if Edge.To = To then
            return True;
         end if;
      end loop;
      return False;
   end Connected;

   -------------------------
   -- Connected_Sub_Graph --
   -------------------------

   procedure Connected_Sub_Graph
     (Container : Graph;
      Start     : Vertex_Type;
      Is_Member : not null access
        function (Vertex : Vertex_Type) return Boolean;
      Result    : out Sub_Graph)
   is
      package Queue_Of_Partials is
        new Ada.Containers.Doubly_Linked_Lists (Index_Type);

      Queue : Queue_Of_Partials.List;

   begin
      Container.Create (Result);
      Queue.Append (Index_Of (Start));
      while not Queue.Is_Empty loop
         declare
            Ix   : constant Index_Type := Queue.First_Element;
         begin
            Queue.Delete_First;
            if Is_Member (Container.Vs (Ix))
              and then not Contains (Result, Ix)
            then
               Append (Result, Ix);
               for Edge of Container.Vertices.Element (Ix).Edges loop
                  Queue.Append (Edge.To);
               end loop;
            end if;
         end;
      end loop;

   end Connected_Sub_Graph;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Graph;
      Vertex    : Vertex_Type)
      return Boolean
   is
   begin
      return Container.Index_Of (Vertex) in Index_Type'Range;
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (Sub : Sub_Graph;
      Index : Index_Type)
      return Boolean
   is
   begin
      return Sub.Vertex_Flags.Element (Index);
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (Collection : Sub_Graph_Collection;
      Index      : Index_Type)
      return Boolean
   is
   begin
      for Sub of Collection.Vector loop
         if Contains (Sub, Index) then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ----------
   -- Cost --
   ----------

   function Cost (P : Path) return Cost_Type is
   begin
      return P.Cost;
   end Cost;

   ------------
   -- Create --
   ------------

   procedure Create
     (Container : Graph'Class;
      Sub       : out Sub_Graph)
   is
   begin
      Sub.Main_Graph := Container'Unchecked_Access;
      Sub.Vertex_List.Clear;
      Sub.Vertex_Flags.Clear;
      for I in 1 .. Container.Last_Vertex_Index loop
         Sub.Vertex_Flags.Append (False);
      end loop;
   end Create;

   ------------------------
   -- Depth_First_Search --
   ------------------------

   procedure Depth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max       : Cost_Type;
      Result    : out Sub_Graph)
   is
      pragma Unreferenced (Max);
      Stack : Index_Lists.List;
   begin
      Container.Create (Result);
      Stack.Append (Start);
      while not Stack.Is_Empty loop
         declare
            Ix : constant Index_Type := Stack.First_Element;
         begin
            Stack.Delete_First;
            if not Contains (Result, Ix) then
               Append (Result, Ix);
               for Edge of Container.Vertices.Element (Ix).Edges loop
                  Stack.Insert (Stack.First, Edge.To);
               end loop;
            end if;
         end;
      end loop;

   end Depth_First_Search;

   ----------
   -- Edge --
   ----------

   function Edge
     (Container : Graph;
      From      : Vertex_Type;
      Index     : Count_Type)
      return Vertex_Type
   is
      Edges : Edge_Lists.List renames
        Container.Vertices.Element (Index_Of (From)).Edges;
      Position : Edge_Lists.Cursor := Edges.First;
   begin
      for I in 2 .. Index loop
         Edge_Lists.Next (Position);
      end loop;
      return Container.Vs (Edge_Lists.Element (Position).To);
   end Edge;

   ---------------
   -- Edge_Cost --
   ---------------

   function Edge_Cost
     (Container    : Graph;
      From, To     : Index_Type)
      return Cost_Type
   is
   begin
      for Edge of Container.Vertices.Element (From).Edges loop
         if Edge.To = To then
            return Edge.Cost;
         end if;
      end loop;
      --  can't happen because of precondition
      raise Program_Error with "invalid call to edge_cost";
   end Edge_Cost;

   ------------------------------
   -- Get_Connected_Components --
   ------------------------------

   procedure Get_Connected_Components
     (Container : Graph'Class;
      Result    : out Sub_Graph_Collection)
   is
   begin
      Result.Vector.Clear;
      for Index in 1 .. Container.Vertices.Last_Index loop
         if not Contains (Result, Index) then
            declare
               Sub : Sub_Graph;
            begin
               Container.Depth_First_Search (Index, Cost_Type'Last, Sub);
               Append (Result, Sub);
            end;
         end if;
      end loop;
   end Get_Connected_Components;

   -------------------
   -- Get_Sub_Graph --
   -------------------

   function Get_Sub_Graph
     (Collection : Sub_Graph_Collection;
      Index      : Positive)
      return Sub_Graph
   is
   begin
      return Collection.Vector (Index);
   end Get_Sub_Graph;

   --------------
   -- Index_Of --
   --------------

   function Index_Of
     (Container : Graph;
      Vertex    : Vertex_Type)
      return Extended_Index
   is
   begin
      for I in Index_Type'First .. Container.Vertices.Last_Index loop
         if Container.Vertices.Element (I).Vertex = Vertex then
            return I;
         end if;
      end loop;
      return Extended_Index'First;
   end Index_Of;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Collection : in out Sub_Graph_Collection;
      Sub        : Sub_Graph)
   is
   begin
      Collection.Vector.Append (Sub);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Graph;
      Process   : not null access procedure (Position : Cursor))
   is
      procedure Local_Process (Position : Vertex_Info_Vectors.Cursor);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process (Position : Vertex_Info_Vectors.Cursor) is
      begin
         Process (Cursor (Position));
      end Local_Process;

   begin
      Container.Vertices.Iterate (Local_Process'Access);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Sub     : Sub_Graph;
      Process : not null access procedure (Vertex : Vertex_Type))
   is
   begin
      for Index of Sub.Vertex_List loop
         Process (Sub.Main_Graph.Vs (Index));
      end loop;
   end Iterate;

   -------------------
   -- Iterate_Edges --
   -------------------

   procedure Iterate_Edges
     (Container : Graph;
      From      : Index_Type;
      Process   : not null access procedure (To : Index_Type;
                                             Cost : Cost_Type))
   is
   begin
      for Edge of Container.Vertices.Element (From).Edges loop
         Process (Edge.To, Edge.Cost);
      end loop;
   end Iterate_Edges;

   -------------------
   -- Iterate_Edges --
   -------------------

   procedure Iterate_Edges
     (Container : Graph;
      From      : Vertex_Type;
      Process   : not null access
        procedure (To : Vertex_Type;
                   Cost : Cost_Type))
   is
      procedure Internal (To : Index_Type;
                          Cost : Cost_Type);

      --------------
      -- Internal --
      --------------

      procedure Internal (To   : Index_Type;
                          Cost : Cost_Type)
      is
      begin
         Process (Container.Vertices.Element (To).Vertex, Cost);
      end Internal;

   begin
      Graph'Class (Container).Iterate_Edges (Index_Of (From), Internal'Access);
   end Iterate_Edges;

   -----------------------
   -- Last_Vertex_Index --
   -----------------------

   function Last_Vertex_Index
     (Container : Graph)
      return Extended_Index
   is
   begin
      return Container.Vertices.Last_Index;
   end Last_Vertex_Index;

   ----------
   -- Next --
   ----------

   function Next
     (Container : Graph;
      P         : Path)
      return Vertex_Type
   is
   begin
      return Container.Vs.Element
        (Index_Lists.Element (Index_Lists.Next (P.List.First)));
   end Next;

   -------------------
   -- Path_Vertices --
   -------------------

   function Path_Vertices
     (Container : Graph;
      P         : Path)
      return Array_Of_Vertices
   is
      pragma Unreferenced (Container);
      Count  : Positive := 1;
   begin
      return Result : Array_Of_Vertices (1 .. Natural (P.List.Length)) do
         for Index of P.List loop
            Result (Count) := Index;
            Count := Count + 1;
         end loop;
      end return;
   end Path_Vertices;

   --------------------
   -- Same_Sub_Graph --
   --------------------

   function Same_Sub_Graph
     (Collection : Sub_Graph_Collection;
      V1, V2     : Index_Type)
      return Boolean
   is
   begin
      for Sub_Graph of Collection.Vector loop
         if Contains (Sub_Graph, V1) then
            return Contains (Sub_Graph, V2);
         elsif Contains (Sub_Graph, V2) then
            return False;
         end if;
      end loop;
      return False;
   end Same_Sub_Graph;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type)
      return Path
   is
      function Cost (From, To : Vertex_Type) return Cost_Type
      is (Container.Edge_Cost (Index_Of (From), Index_Of (To)));
   begin
      return Container.Shortest_Path (From, To, Cost'Access);
   end Shortest_Path;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type;
      Test_Vertex : not null access
        function (Vertex : Vertex_Type) return Boolean)
      return Path
   is

      pragma Warnings (Off);

      function Passable
        (From, To : Vertex_Type)
         return Boolean
      is (Test_Vertex (To));

      function Cost (From, To : Vertex_Type) return Cost_Type is (1.0);
      function Estimate (From, To : Vertex_Type) return Cost_Type is (0.0);

   begin
      return Container.Shortest_Path
        (From, To, Passable'Access, Cost'Access, Estimate'Access);
   end Shortest_Path;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Container      : Graph'Class;
      Start, Finish  : Index_Type;
      Passable       : not null access
        function (From, To : Vertex_Type) return Boolean;
      Cost           : not null access
        function (From, To : Vertex_Type) return Cost_Type;
      Estimate       : not null access
        function (From, To : Vertex_Type) return Cost_Type)
      return Path
   is

      package Frontier_Queues is
        new WL.Heaps
          (Key_Type     => Cost_Type,
           Element_Type => Index_Type,
           "<"          => ">");

      Sentinel : constant Extended_Index := Extended_Index'First;

      type Visited_Info is
         record
            Visited     : Boolean := False;
            Came_From   : Extended_Index := Sentinel;
            Cost_So_Far : Cost_Type := 0.0;
            Rest_Cost   : Cost_Type := 0.0;
         end record;

      Visited : array (1 .. Container.Vertices.Last_Index) of Visited_Info;
      Frontier : Frontier_Queues.Heap;

   begin

      Frontier.Insert (0.0, Start);
      Visited (Start).Visited := True;
      Visited (Start).Rest_Cost :=
        Estimate (Container.Vs (Start), Container.Vs (Finish));

      while not Frontier.Is_Empty loop
         declare
            Current      : constant Index_Type :=
                             Frontier.First_Element;
            Current_V    : constant Vertex_Type :=
                             Container.Vs.Element (Current);
            Current_Cost : constant Cost_Type :=
                             Visited (Current).Cost_So_Far;
         begin
            Frontier.Delete_First;

            exit when Current = Finish;

            for Edge of Container.Vertices.Element (Current).Edges loop
               declare
                  Next_V : constant Vertex_Type :=
                             Container.Vs.Element (Edge.To);
               begin
                  if Passable (Current_V, Next_V) then
                     declare
                        New_Cost : constant Cost_Type :=
                                     Current_Cost + Cost (Current_V, Next_V);
                        Info     : Visited_Info renames Visited (Edge.To);
                     begin
                        if not Info.Visited
                          or else New_Cost < Info.Cost_So_Far
                        then
                           if Info.Visited then
                              Frontier.Replace
                                (New_Cost + Info.Rest_Cost, Edge.To);
                           else
                              Info.Rest_Cost :=
                                Estimate (Next_V, Container.Vs (Finish));
                              Frontier.Insert
                                (New_Cost + Info.Rest_Cost, Edge.To);
                              Info.Visited := True;
                           end if;
                           Info.Cost_So_Far := New_Cost;
                           Info.Came_From := Current;
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;

      declare
         Result : Path;
         It     : Index_Type := Finish;
      begin
         Result.Cost := Visited (It).Cost_So_Far;
         while Visited (It).Came_From /= Sentinel loop
            if Result.List.Is_Empty then
               Result.List.Append (It);
            else
               Result.List.Insert (Result.List.First, It);
            end if;
            It := Visited (It).Came_From;
         end loop;
         return Result;
      end;

   end Shortest_Path;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type;
      Cost      : not null access
        function (From, To : Vertex_Type) return Cost_Type)
      return Path
   is
      function Passable (From, To : Vertex_Type) return Boolean is (True);
      function Estimate (From, To : Vertex_Type) return Cost_Type is (0.0);
   begin
      return Container.Shortest_Path
        (From, To, Passable'Access, Cost, Estimate'Access);
   end Shortest_Path;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type;
      Cost      : not null access
        function (From, To : Vertex_Type) return Cost_Type;
      Estimate  : not null access
        function (From, To : Vertex_Type) return Cost_Type)
      return Path
   is
      function Passable (From, To : Vertex_Type) return Boolean is (True);
   begin
      return Container.Shortest_Path
        (From, To, Passable'Access, Cost, Estimate);
   end Shortest_Path;

   ---------------------
   -- Sub_Graph_Count --
   ---------------------

   function Sub_Graph_Count
     (Collection : Sub_Graph_Collection)
      return Natural
   is
   begin
      return Collection.Vector.Last_Index;
   end Sub_Graph_Count;

   ------------
   -- Vertex --
   ------------

   function Vertex
     (Container : Graph;
      Index     : Index_Type)
      return Vertex_Type
   is
   begin
      return Container.Vs (Index);
   end Vertex;

   ------------------
   -- Vertex_Count --
   ------------------

   function Vertex_Count (P : Path) return Extended_Index is
   begin
      return Extended_Index (P.List.Length);
   end Vertex_Count;

end WL.Graphs;
