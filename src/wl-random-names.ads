private with Ada.Containers.Vectors;

package WL.Random.Names is

   type Name_Generator is private;

   procedure Load_Lexicon
     (Generator       :    out Name_Generator;
      Vowels_Path     : String;
      Consonants_Path : String);

   function Load_Lexicon
     (Vowels_Path     : String;
      Consonants_Path : String)
     return Name_Generator;

   function Random_Name
     (Generator : Name_Generator)
      return String;

private

   type Lexeme_Info is
      record
         Lexeme : String (1 .. 3) := (others => ' ');
         Can_Middle : Boolean := True;
         Can_Begin : Boolean := True;
         Can_End : Boolean := True;
      end record;

   package Lexeme_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Lexeme_Info);

   type Name_Generator is
      record
         Vowels     : Lexeme_Info_Vectors.Vector;
         Consonants : Lexeme_Info_Vectors.Vector;
      end record;

end WL.Random.Names;
