with Ada.Containers.Hashed_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package WL.Guids.Maps is

   package Guid_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Guid,
        Element_Type    => Element_Type,
        Hash            => Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   subtype Map is Guid_Maps.Map;

   function Element (Position : Guid_Maps.Cursor) return Element_Type
                     renames Guid_Maps.Element;

end WL.Guids.Maps;
