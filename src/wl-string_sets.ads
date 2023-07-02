private with WL.String_Maps;

package WL.String_Sets is

   pragma Preelaborate;

   type Set is tagged private;

   Empty_Set : constant Set;

   function Is_Empty
     (Container : Set)
      return Boolean;

   function Count
     (Container : Set)
      return Natural;

   function Contains
     (Container : Set;
      Element   : String)
      return Boolean;

   function First_Element
     (Container : Set)
      return String;

   procedure Clear
     (Container : in out Set);

   procedure Include
     (Container : in out Set;
      Element   : String);

   procedure Delete
     (Container : in out Set;
      Element   : String);

   procedure Iterate
     (Container : Set;
      Process   : not null access
        procedure (Item : String));

private

   package Sets is new WL.String_Maps (Boolean);

   type Set is tagged
      record
         Container : Sets.Map;
      end record;

   Empty_Set : constant Set := (Container => <>);

   function Contains
     (Container : Set;
      Element   : String)
      return Boolean
   is (Container.Container.Contains (Element));

   function Is_Empty
     (Container : Set)
      return Boolean
   is (Container.Container.Is_Empty);

   function Count
     (Container : Set)
      return Natural
   is (Natural (Container.Container.Length));

   function First_Element
     (Container : Set)
      return String
   is (Sets.Key (Container.Container.First));

end WL.String_Sets;
