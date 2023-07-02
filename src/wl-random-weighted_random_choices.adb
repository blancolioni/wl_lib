package body WL.Random.Weighted_Random_Choices is

   ------------
   -- Choose --
   ------------

   function Choose
     (Set : Weighted_Choice_Set)
      return Element_Type
   is
      X : Positive := WL.Random.Random_Number (1, Set.Total_Score);
   begin
      for Choice of Set.Vector loop
         if X <= Choice.Score then
            return Choice.Element;
         else
            X := X - Choice.Score;
         end if;
      end loop;

      raise Program_Error;

   end Choose;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Set    : in out Weighted_Choice_Set;
      Item   : Element_Type)
   is
      Found : Boolean := False;
   begin
      for I in 1 .. Set.Vector.Last_Index loop
         if Set.Vector (I).Element = Item then
            Found := True;
         elsif Found then
            Set.Vector (I - 1) := Set.Vector (I);
         end if;
      end loop;

      if Found then
         Set.Vector.Delete_Last;
      end if;
   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Set    : in out Weighted_Choice_Set;
      Item   : Element_Type;
      Score  : Natural)
   is
   begin
      if Score > 0 then
         Set.Vector.Append (Weighted_Element'(Item, Score));
         Set.Total_Score := Set.Total_Score + Score;
      end if;
   end Insert;

end WL.Random.Weighted_Random_Choices;
