with Ada.Characters.Handling;
with Ada.Text_IO;

package body WL.Random.Names is

   ------------------
   -- Load_Lexicon --
   ------------------

   function Load_Lexicon
     (Vowels_Path     : String;
      Consonants_Path : String)
      return Name_Generator
   is
   begin
      return Gen : Name_Generator do
         Load_Lexicon (Gen, Vowels_Path, Consonants_Path);
      end return;
   end Load_Lexicon;

   ------------------
   -- Load_Lexicon --
   ------------------

   procedure Load_Lexicon
     (Generator       :    out Name_Generator;
      Vowels_Path     : String;
      Consonants_Path : String)
   is
      procedure Load_Info
        (Path   : String;
         Target : in out Lexeme_Info_Vectors.Vector'Class);

      ---------------
      -- Load_Info --
      ---------------

      procedure Load_Info
        (Path   : String;
         Target : in out Lexeme_Info_Vectors.Vector'Class)
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, Path);
         while not End_Of_File (File) loop
            declare
               Full_Line : constant String := Get_Line (File);
               Last_CR   : constant Boolean :=
                             Full_Line'Length > 0
                                 and then Character'Pos
                                   (Full_Line (Full_Line'Last)) = 13;
               Line      : constant String :=
                             (if Last_CR
                              then Full_Line
                                (Full_Line'First .. Full_Line'Last - 1)
                              else Full_Line);
               Lex  : constant String := Line (Line'First + 1 .. Line'Last);
               Flags : constant Natural :=
                         Natural'Value (Line (Line'First .. Line'First));
               Info  : Lexeme_Info;
            begin
               Info.Can_End := Flags mod 2 = 1;
               Info.Can_Begin := (Flags / 2) mod 2 = 1;
               Info.Can_Middle := (Flags / 4) mod 2 = 1;
               Info.Lexeme (1 .. Lex'Length) := Lex;
               Target.Append (Info);
            end;
         end loop;
         Close (File);
      end Load_Info;

   begin
      Load_Info (Vowels_Path, Generator.Vowels);
      Load_Info (Consonants_Path, Generator.Consonants);
   end Load_Lexicon;

   -----------------
   -- Random_Name --
   -----------------

   function Random_Name
     (Generator : Name_Generator)
      return String
   is
      Next_Is_Vowel : Boolean := Random_Number (1, 2) = 1;
      Syllable_Count : constant Positive := Random_Number (3, 5);
      Name_Length    : Natural := 0;
      Name           : String (1 .. Syllable_Count * 3);
   begin
      for I in 1 .. Syllable_Count loop
         loop
            declare
               Info : constant Lexeme_Info :=
                        (if Next_Is_Vowel
                         then Generator.Vowels
                           (Random_Number (1, Generator.Vowels.Last_Index))
                         else Generator.Consonants
                           (Random_Number
                              (1, Generator.Consonants.Last_Index)));
               OK   : Boolean;
            begin
               if I = 1 then
                  OK := Info.Can_Begin;
               elsif I = Syllable_Count then
                  OK := Info.Can_End;
               else
                  OK := Info.Can_Middle;
               end if;

               if OK then
                  for I in Info.Lexeme'Range loop
                     exit when Info.Lexeme (I) = ' ';
                     Name_Length := Name_Length + 1;
                     Name (Name_Length) := Info.Lexeme (I);
                  end loop;
                  exit;
               end if;
            end;
         end loop;

         Next_Is_Vowel := not Next_Is_Vowel;

      end loop;

      Name (1) := Ada.Characters.Handling.To_Upper (Name (1));
      return Name (1 .. Name_Length);

   end Random_Name;

end WL.Random.Names;
