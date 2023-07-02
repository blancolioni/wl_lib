private with Ada.Containers.Vectors;

generic
   type Key_Type is private;
   type Element_Type is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package WL.Heaps is

   type Heap is tagged private;

   function Empty_Heap return Heap;

   function Is_Empty
     (Container : Heap)
      return Boolean;

   function Length
     (Container : Heap)
      return Natural;

   procedure Insert
     (Container : in out Heap;
      Key       : Key_Type;
      Element   : Element_Type);

   procedure Replace
     (Container : in out Heap;
      Key       : Key_Type;
      Element   : Element_Type);

   function First_Element
     (Container : Heap)
      return Element_Type;

   function First_Key
     (Container : Heap)
      return Key_Type;

   procedure Delete_First
     (Container : in out Heap);

   procedure Iterate
     (Container : Heap;
      Process   : not null access
        procedure (Key : Key_Type;
                   Element : Element_Type));

private

   type Heap_Element is
      record
         Key : Key_Type;
         Element : Element_Type;
      end record;

   package Heap_Vectors is
     new Ada.Containers.Vectors (Positive, Heap_Element);

   type Heap is tagged
      record
         Vector : Heap_Vectors.Vector;
      end record;

   function Empty_Heap return Heap
   is (Vector => Heap_Vectors.Empty_Vector);

   function Length
     (Container : Heap)
      return Natural
   is (Natural (Container.Vector.Length));

   function First_Element
     (Container : Heap)
      return Element_Type
   is (Container.Vector.First_Element.Element);

   function First_Key
     (Container : Heap)
      return Key_Type
   is (Container.Vector.First_Element.Key);

end WL.Heaps;
