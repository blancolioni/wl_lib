package body WL.String_Sets is

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Container : in out Set)
   is
   begin
      Container.Container.Clear;
   end Clear;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Set;
      Element   : String)
   is
   begin
      if Container.Contains (Element) then
         Container.Container.Delete (Element);
      end if;
   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Include
     (Container : in out Set;
      Element   : String)
   is
   begin
      if not Container.Contains (Element) then
         Container.Container.Insert (Element, True);
      end if;
   end Include;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Set;
      Process   : not null access
        procedure (Item : String))
   is
   begin
      for Item in Container.Container.Iterate loop
         Process (Sets.Key (Item));
      end loop;
   end Iterate;

end WL.String_Sets;
