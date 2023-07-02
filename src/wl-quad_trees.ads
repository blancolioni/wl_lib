with Ada.Streams;

generic
   type Property_Type is private;
   No_Property : Property_Type;
   type Rectangle is
     array (Natural range <>, Natural range <>) of Property_Type;
   with function To_String (Item : Property_Type) return String;
   with function From_String (Item : String) return Property_Type;
package WL.Quad_Trees is

   type Quad_Tree is tagged private;

   function Width (Tree : Quad_Tree) return Natural;
   function Height (Tree : Quad_Tree) return Natural;

   function Property (Tree : Quad_Tree;
                      X, Y : Natural)
                     return Property_Type;

   procedure Read_Properties (Tree             : Quad_Tree;
                              Result           : out Rectangle);

   function Create_Tree (Width, Height   : Natural;
                         Get_XY_Prop   : not null access
                         function (X, Y : Natural) return Property_Type)
                        return Quad_Tree;

   procedure Write (Tree   : Quad_Tree;
                    Path   : String);

   function Read (Path : String) return Quad_Tree;

private

   type Quad_Tree_Node (Leaf_Node : Boolean);

   type Quad_Tree_Node_Access is access Quad_Tree_Node;

   type Quad_Tree is tagged
      record
         Width, Height : Natural;
         Top : Quad_Tree_Node_Access;
      end record;

   procedure Read_Quad_Tree
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Quad_Tree);

   procedure Write_Quad_Tree
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Quad_Tree);

   for Quad_Tree'Read  use Read_Quad_Tree;
   for Quad_Tree'Write use Write_Quad_Tree;

end WL.Quad_Trees;
