with Ada.Containers;

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

generic
   type Index_Type is range <>;
   type Vertex_Type is private;
   type Cost_Type is digits <>;

   Default_Cost  : Cost_Type := 1.0;

   with function Index_Of (Vertex : Vertex_Type) return Index_Type;
   with function "=" (Left, Right : Vertex_Type) return Boolean is <>;
package WL.Graphs is

   subtype Count_Type is Ada.Containers.Count_Type;

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
     Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   type Graph is tagged private;

   function Is_Empty
     (Container : Graph)
      return Boolean;

   function Vertex
     (Container : Graph;
      Index     : Index_Type)
      return Vertex_Type
     with Inline;

   function Contains
     (Container : Graph;
      Vertex    : Vertex_Type)
      return Boolean;

   function Last_Vertex_Index
     (Container : Graph)
      return Extended_Index;

   procedure Append
     (Container : in out Graph;
      Vertex    : Vertex_Type)
     with Pre => not Container.Contains (Vertex);

   function Edge_Count
     (Container : Graph;
      Vertex    : Index_Type)
      return Count_Type;

   function Connected
     (Container    : Graph;
      From, To     : Index_Type)
      return Boolean;

   function Edge_Cost
     (Container    : Graph;
      From, To     : Index_Type)
      return Cost_Type
     with Pre => Container.Connected (From, To);

   procedure Connect
     (Container : in out Graph'Class;
      From, To  : Index_Type;
      Cost      : Cost_Type := Default_Cost)
     with Pre => not Container.Connected (From, To);

   type Cursor is private;

   procedure Iterate
     (Container : Graph;
      Process   : not null access procedure (Position : Cursor));

   procedure Iterate_Edges
     (Container : Graph;
      From      : Index_Type;
      Process   : not null access procedure (To : Index_Type;
                                             Cost : Cost_Type));

   procedure Iterate_Edges
     (Container : Graph;
      From      : Vertex_Type;
      Process   : not null access
        procedure (To : Vertex_Type;
                   Cost : Cost_Type));

   function Edge
     (Container : Graph;
      From      : Vertex_Type;
      Index     : Count_Type)
      return Vertex_Type;

   function Breadth_First_Search
     (Container : Graph;
      Start     : Vertex_Type;
      Test      : not null access
        function (Vertex : Vertex_Type) return Boolean)
      return Vertex_Type;

   type Sub_Graph is private;

   procedure Create
     (Container : Graph'Class;
      Sub       : out Sub_Graph);

   procedure Append
     (Sub   : in out Sub_Graph;
      Index : Index_Type);

   function Contains
     (Sub : Sub_Graph;
      Index : Index_Type)
      return Boolean;

   procedure Depth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max       : Cost_Type;
      Result    : out Sub_Graph);

   procedure Breadth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max       : Cost_Type;
      Result    : out Sub_Graph);

   procedure Breadth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Max_Steps : Count_Type;
      Result    : out Sub_Graph);

   procedure Breadth_First_Search
     (Container : Graph;
      Start     : Index_Type;
      Test      : not null access
        function (Vertex : Vertex_Type) return Boolean;
      Result    : out Sub_Graph);

   procedure Connected_Sub_Graph
     (Container : Graph;
      Start     : Vertex_Type;
      Is_Member : not null access
        function (Vertex : Vertex_Type) return Boolean;
      Result    : out Sub_Graph);

   procedure Iterate
     (Sub     : Sub_Graph;
      Process : not null access procedure (Vertex : Vertex_Type));

   type Sub_Graph_Collection is private;

   function Contains
     (Collection : Sub_Graph_Collection;
      Index      : Index_Type)
      return Boolean;

   procedure Append
     (Collection : in out Sub_Graph_Collection;
      Sub        : Sub_Graph);

   function Sub_Graph_Count
     (Collection : Sub_Graph_Collection)
      return Natural;

   function Get_Sub_Graph
     (Collection : Sub_Graph_Collection;
      Index      : Positive)
      return Sub_Graph
     with Pre => Index <= Sub_Graph_Count (Collection);

   function Same_Sub_Graph
     (Collection : Sub_Graph_Collection;
      V1, V2     : Index_Type)
      return Boolean;

   procedure Get_Connected_Components
     (Container : Graph'Class;
      Result    : out Sub_Graph_Collection);

   procedure Insert
     (Collection : in out Sub_Graph_Collection;
      Sub        : Sub_Graph);

   type Path is private;

   function Cost (P : Path) return Cost_Type;
   function Vertex_Count (P : Path) return Extended_Index;
   function Next (Container : Graph;
                  P         : Path)
                  return Vertex_Type
     with Pre => Vertex_Count (P) > 1;

   type Array_Of_Vertices is array (Positive range <>) of Index_Type;

   function Path_Vertices
     (Container : Graph;
      P         : Path)
      return Array_Of_Vertices;

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type)
      return Path;

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type;
      Test_Vertex : not null access
        function (Vertex : Vertex_Type) return Boolean)
      return Path;

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type;
      Cost      : not null access
        function (From, To : Vertex_Type) return Cost_Type)
      return Path;

   function Shortest_Path
     (Container : Graph'Class;
      From, To  : Index_Type;
      Cost      : not null access
        function (From, To : Vertex_Type) return Cost_Type;
      Estimate  : not null access
        function (From, To : Vertex_Type) return Cost_Type)
      return Path;

   function Shortest_Path
     (Container      : Graph'Class;
      Start, Finish  : Index_Type;
      Passable       : not null access
        function (From, To : Vertex_Type) return Boolean;
      Cost           : not null access
        function (From, To : Vertex_Type) return Cost_Type;
      Estimate       : not null access
        function (From, To : Vertex_Type) return Cost_Type)
      return Path;

   procedure Breadth_First_Scan
     (Container : Graph;
      Start     : Index_Type;
      Process   : not null access
        procedure (Path_To : Path));

private

   type Edge_Type is
      record
         To   : Index_Type;
         Cost : Cost_Type;
      end record;

   package Edge_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Edge_Type);

   type Vertex_Info is
      record
         Vertex : Vertex_Type;
         Index  : Index_Type;
         Edges  : Edge_Lists.List;
      end record;

   package Vertex_Info_Vectors is
     new Ada.Containers.Vectors (Index_Type, Vertex_Info);

   package Vertex_Vectors is
     new Ada.Containers.Vectors (Index_Type, Vertex_Type);

   type Graph is tagged
      record
         Vertices : Vertex_Info_Vectors.Vector;
         Vs       : Vertex_Vectors.Vector;
      end record;

   type Cursor is new Vertex_Info_Vectors.Cursor;

   package Index_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Index_Type);

   package Index_Flag_Vectors is
     new Ada.Containers.Vectors (Index_Type, Boolean);

   type Sub_Graph is
      record
         Main_Graph   : access constant Graph'Class;
         Vertex_List  : Index_Lists.List;
         Vertex_Flags : Index_Flag_Vectors.Vector;
      end record;

   package Sub_Graph_Vectors is
     new Ada.Containers.Vectors (Positive, Sub_Graph);

   type Sub_Graph_Collection is
      record
         Vector : Sub_Graph_Vectors.Vector;
      end record;

   type Path is
      record
         Cost : Cost_Type := 0.0;
         List : Index_Lists.List;
      end record;

   function Index_Of
     (Container : Graph;
      Vertex    : Vertex_Type)
      return Extended_Index;

   function Edge_Count
     (Container : Graph;
      Vertex    : Index_Type)
      return Count_Type
   is (Container.Vertices.Element (Vertex).Edges.Length);

   function Is_Empty
     (Container : Graph)
      return Boolean
   is (Container.Vs.Is_Empty);

end WL.Graphs;
