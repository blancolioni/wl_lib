package WL.Random.Height_Maps is

   type Height_Array is array (Positive range <>) of Integer;

   type Frequency_Map is array (Integer range <>) of Natural;

   type Neighbour_Array is array (Positive range <>) of Positive;

   procedure Generate_Height_Map
     (Heights     : out Height_Array;
      Frequencies : Frequency_Map;
      Smoothing   : Natural;
      Neighbours  : not null access
        function (Index : Positive)
      return Neighbour_Array);

end WL.Random.Height_Maps;
