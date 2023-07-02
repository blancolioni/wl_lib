package body WL.Heaps is

   procedure Heapify
     (Container : in out Heap'Class);

   procedure Sift_Down
     (Container : in out Heap'Class;
      Start     : Positive);

   procedure Sift_Up
     (Container : in out Heap'Class;
      Start     : Positive;
      Node      : Positive);

   function Key
     (Container : Heap'Class;
      Index     : Positive)
      return Key_Type
   is (Container.Vector.Element (Index).Key);

   procedure Swap
     (Container        : in out Heap'Class;
      Index_1, Index_2 : Positive);

   function Left_Child
     (Node : Positive)
      return Positive
   is (Node * 2);

   function Right_Child
     (Node : Positive)
      return Positive
   is (Node * 2 + 1)
     with Unreferenced;

   function Parent
     (Node : Positive)
      return Natural
   is (Node / 2);

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Container : in out Heap)
   is
   begin
      Container.Vector.Replace_Element (1, Container.Vector.Last_Element);
      Container.Vector.Delete_Last;
      Container.Sift_Down (1);
   end Delete_First;

   -------------
   -- Heapify --
   -------------

   procedure Heapify
     (Container : in out Heap'Class)
   is
      Start : Natural := Parent (Container.Vector.Last_Index);
   begin
      while Start > 0 loop
         Container.Sift_Down (Start);
         Start := Start - 1;
      end loop;
   end Heapify;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Heap;
      Key       : Key_Type;
      Element   : Element_Type)
   is
   begin
      Container.Vector.Append (Heap_Element'(Key, Element));
      Container.Sift_Up (1, Container.Vector.Last_Index);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Container : Heap)
      return Boolean
   is
   begin
      return Container.Vector.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Heap;
      Process   : not null access
        procedure (Key : Key_Type;
                   Element : Element_Type))
   is
   begin
      for Item of Container.Vector loop
         Process (Item.Key, Item.Element);
      end loop;
   end Iterate;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Heap;
      Key       : Key_Type;
      Element   : Element_Type)
   is
      Found : Boolean := False;
   begin
      for I in 1 .. Container.Vector.Last_Index loop
         if Container.Vector.Element (I).Element = Element then
            Found := True;
         elsif Found then
            Container.Vector.Replace_Element
              (I - 1, Container.Vector.Element (I));
         end if;
      end loop;

      pragma Assert (Found);
      Container.Vector.Replace_Element (Container.Vector.Last_Index,
                                        (Key, Element));
      Container.Heapify;
   end Replace;

   ---------------
   -- Sift_Down --
   ---------------

   procedure Sift_Down
     (Container : in out Heap'Class;
      Start     : Positive)
   is
      Root  : Positive := Start;
      Child : Positive;
      Swap  : Positive;
      Last  : constant Natural := Container.Vector.Last_Index;
   begin
      while Left_Child (Root) <= Last loop
         Child := Left_Child (Root);
         Swap := Root;

         if Container.Key (Swap) < Container.Key (Child) then
            Swap := Child;
         end if;

         if Child + 1 <= Last
           and then Container.Key (Swap) < Container.Key (Child + 1)
         then
            Swap := Child + 1;
         end if;

         if Swap = Root then
            return;
         end if;

         Container.Swap (Swap, Root);

         Root := Swap;
      end loop;
   end Sift_Down;

   -------------
   -- Sift_Up --
   -------------

   procedure Sift_Up
     (Container : in out Heap'Class;
      Start     : Positive;
      Node      : Positive)
   is
      Child : Positive := Node;
   begin
      while Child > Start loop
         declare
            P : constant Positive := Parent (Child);
         begin
            if Container.Key (P) < Container.Key (Child) then
               Container.Swap (P, Child);
               Child := P;
            else
               exit;
            end if;
         end;
      end loop;
   end Sift_Up;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Container        : in out Heap'Class;
      Index_1, Index_2 : Positive)
   is
      X : constant Heap_Element := Container.Vector (Index_1);
   begin
      Container.Vector.Replace_Element
        (Index_1, Container.Vector.Element (Index_2));

      Container.Vector.Replace_Element
        (Index_2, X);
   end Swap;

end WL.Heaps;
