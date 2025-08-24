pragma Ada_2012;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Database_Operations; use Database_Operations;

package body Test_Runner is

   --  Define a vector to store clean string tokens
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);

   --  Global test counters
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   --  Enhanced trimmed conversion functions
   function Trimmed_Positive (Token : String) return Positive is
      Clean_Token : String (1 .. Token'Length);
      Clean_Index : Natural := 0;
   begin
      -- Extract only digits, removing any invisible characters
      for I in Token'Range loop
         if Token (I) in '0' .. '9' then
            Clean_Index := Clean_Index + 1;
            Clean_Token (Clean_Index) := Token (I);
         end if;
      end loop;

      if Clean_Index = 0 then
         raise Constraint_Error with "No valid digits in token: " & Token;
      end if;

      return Positive'Value (Clean_Token (1 .. Clean_Index));
   exception
      when E : others =>
         raise Constraint_Error with "Failed to convert '" & Token & 
                                   "' to Positive: " & Ada.Exceptions.Exception_Message (E);
   end Trimmed_Positive;

   function Trimmed_Natural (Token : String) return Natural is
      Clean_Token : String (1 .. Token'Length);
      Clean_Index : Natural := 0;
   begin
      -- Extract only digits, removing any invisible characters
      for I in Token'Range loop
         if Token (I) in '0' .. '9' then
            Clean_Index := Clean_Index + 1;
            Clean_Token (Clean_Index) := Token (I);
         end if;
      end loop;

      if Clean_Index = 0 then
         raise Constraint_Error with "No valid digits in token: " & Token;
      end if;

      return Natural'Value (Clean_Token (1 .. Clean_Index));
   exception
      when E : others =>
         raise Constraint_Error with "Failed to convert '" & Token & 
                                   "' to Natural: " & Ada.Exceptions.Exception_Message (E);
   end Trimmed_Natural;

   function Clean_String_Token (Token : String) return String is
      Result : String (1 .. Token'Length);
      Result_Index : Natural := 0;
   begin
      -- Remove spaces, tabs, and control characters, keep alphanumeric and underscores
      for I in Token'Range loop
         if Token (I) in 'A' .. 'Z' or Token (I) in 'a' .. 'z' or 
            Token (I) in '0' .. '9' or Token (I) = '_' then
            Result_Index := Result_Index + 1;
            Result (Result_Index) := Token (I);
         end if;
      end loop;
      
      return Result (1 .. Result_Index);
   end Clean_String_Token;

   procedure Log_Test_Result
     (Success : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Success then
         Pass_Count := Pass_Count + 1;
         Ada.Text_IO.Put_Line ("✓ PASS: " & Message);
      else
         Ada.Text_IO.Put_Line ("✗ FAIL: " & Message);
      end if;
   end Log_Test_Result;

   procedure Execute_Add_Airport_Test
     (Name, Location : String; Capacity : Positive) is
   begin
      Add_Airport (Name, Location, Capacity);
      Log_Test_Result (True, "Added airport " & Name);
   exception
      when Duplicate_Record =>
         Log_Test_Result (False, "Duplicate airport " & Name);
      when others =>
         Log_Test_Result (False, "Error adding airport " & Name);
   end Execute_Add_Airport_Test;

   procedure Execute_Add_Controller_Test
     (License, Name : String; Experience : Natural) is
   begin
      Add_Controller (License, Name, Experience);
      Log_Test_Result (True, "Added controller " & License);
   exception
      when Duplicate_Record =>
         Log_Test_Result (False, "Duplicate controller " & License);
      when others =>
         Log_Test_Result (False, "Error adding controller " & License);
   end Execute_Add_Controller_Test;

   procedure Execute_Add_Flight_Test
     (Identifier, Origin, Destination : String) is
   begin
      Add_Flight (Identifier, Origin, Destination);
      Log_Test_Result (True, "Added flight " & Identifier);
   exception
      when Duplicate_Record =>
         Log_Test_Result (False, "Duplicate flight " & Identifier);
      when others =>
         Log_Test_Result (False, "Error adding flight " & Identifier);
   end Execute_Add_Flight_Test;

   procedure Execute_Delete_Test
     (Entity_Type, Identifier : String) is
   begin
      if Entity_Type = "AIRPORT" then
         Delete_Airport (Identifier);
         Log_Test_Result (True, "Deleted airport " & Identifier);
      elsif Entity_Type = "CONTROLLER" then
         Delete_Controller (Identifier);
         Log_Test_Result (True, "Deleted controller " & Identifier);
      elsif Entity_Type = "FLIGHT" then
         Delete_Flight (Identifier);
         Log_Test_Result (True, "Deleted flight " & Identifier);
      else
         Log_Test_Result (False, "Unknown entity type " & Entity_Type);
      end if;
   exception
      when Record_Not_Found =>
         Log_Test_Result (False, Entity_Type & " not found: " & Identifier);
      when others =>
         Log_Test_Result (False, "Error deleting " & Entity_Type & 
                          ": " & Identifier);
   end Execute_Delete_Test;

   procedure Execute_Verify_Count_Test
     (Entity_Type : String; Expected_Count : Natural) is
      Actual_Count : Natural;
   begin
      if Entity_Type = "AIRPORT" then
         Actual_Count := Natural (List_Airports.Length);
      elsif Entity_Type = "CONTROLLER" then
         Actual_Count := Natural (List_Controllers.Length);
      elsif Entity_Type = "FLIGHT" then
         Actual_Count := Natural (List_Flights.Length);
      else
         Log_Test_Result (False, "Unknown entity type for count: " & 
                          Entity_Type);
         return;
      end if;

      if Expected_Count = Actual_Count then
         Log_Test_Result (True, Entity_Type & " count correct (" &
                        Expected_Count'Image & ")");
      else
         Log_Test_Result (False, Entity_Type & " count - expected " &
                        Expected_Count'Image & ", got " & 
                        Actual_Count'Image);
      end if;
   end Execute_Verify_Count_Test;

   function Split_Line_To_Vector 
     (Line : String) return String_Vectors.Vector is
      Result : String_Vectors.Vector;
      Current_Token : Unbounded_String;
      In_Token : Boolean := False;
   begin
      -- Parse character by character, handling multiple delimiters
      for I in Line'Range loop
         if Line (I) = ' ' or Line (I) = ',' or Line (I) = ASCII.HT then
            -- Found delimiter
            if In_Token then
               -- End current token and clean it thoroughly
               declare
                  Raw_Token : constant String := To_String (Current_Token);
                  Clean_Token : constant String := Clean_String_Token (Raw_Token);
               begin
                  if Clean_Token'Length > 0 then
                     Result.Append (To_Unbounded_String (Clean_Token));
                  end if;
               end;
               Current_Token := Null_Unbounded_String;
               In_Token := False;
            end if;
         elsif Line (I) = '#' then
            -- Comment found, stop parsing
            if In_Token then
               declare
                  Raw_Token : constant String := To_String (Current_Token);
                  Clean_Token : constant String := Clean_String_Token (Raw_Token);
               begin
                  if Clean_Token'Length > 0 then
                     Result.Append (To_Unbounded_String (Clean_Token));
                  end if;
               end;
            end if;
            exit;
         else
            -- Regular character
            if not In_Token then
               In_Token := True;
            end if;
            Append (Current_Token, Line (I));
         end if;
      end loop;

      -- Handle last token
      if In_Token then
         declare
            Raw_Token : constant String := To_String (Current_Token);
            Clean_Token : constant String := Clean_String_Token (Raw_Token);
         begin
            if Clean_Token'Length > 0 then
               Result.Append (To_Unbounded_String (Clean_Token));
            end if;
         end;
      end if;

      return Result;
   end Split_Line_To_Vector;

   procedure Process_Command_Vector 
     (Tokens : String_Vectors.Vector) is
   begin
      -- Skip empty lines
      if Tokens.Is_Empty then
         return;
      end if;

      declare
         Command : constant String := To_String (Tokens.Element (0));
      begin
         -- Process commands with comprehensive error handling
         if Command = "ADD_AIRPORT" and Natural (Tokens.Length) = 4 then
            begin
               Execute_Add_Airport_Test
                 (To_String (Tokens.Element (1)),
                  To_String (Tokens.Element (2)),
                  Trimmed_Positive (To_String (Tokens.Element (3))));
            exception
               when E : others =>
                  Log_Test_Result (False, "ADD_AIRPORT error: " &
                                  Ada.Exceptions.Exception_Message (E));
            end;

         elsif Command = "ADD_CONTROLLER" and Natural (Tokens.Length) = 4 then
            begin
               Execute_Add_Controller_Test
                 (To_String (Tokens.Element (1)),
                  To_String (Tokens.Element (2)),
                  Trimmed_Natural (To_String (Tokens.Element (3))));
            exception
               when E : others =>
                  Log_Test_Result (False, "ADD_CONTROLLER error: " &
                                  Ada.Exceptions.Exception_Message (E));
            end;

         elsif Command = "ADD_FLIGHT" and Natural (Tokens.Length) = 4 then
            begin
               Execute_Add_Flight_Test
                 (To_String (Tokens.Element (1)),
                  To_String (Tokens.Element (2)),
                  To_String (Tokens.Element (3)));
            exception
               when E : others =>
                  Log_Test_Result (False, "ADD_FLIGHT error: " &
                                  Ada.Exceptions.Exception_Message (E));
            end;

         elsif Command = "DELETE_AIRPORT" and Natural (Tokens.Length) = 2 then
            Execute_Delete_Test ("AIRPORT", To_String (Tokens.Element (1)));

         elsif Command = "DELETE_CONTROLLER" and Natural (Tokens.Length) = 2 then
            Execute_Delete_Test ("CONTROLLER", To_String (Tokens.Element (1)));

         elsif Command = "DELETE_FLIGHT" and Natural (Tokens.Length) = 2 then
            Execute_Delete_Test ("FLIGHT", To_String (Tokens.Element (1)));

         elsif Command = "VERIFY_AIRPORT_COUNT" and Natural (Tokens.Length) = 2 then
            begin
               Execute_Verify_Count_Test 
                 ("AIRPORT", 
                  Trimmed_Natural (To_String (Tokens.Element (1))));
            exception
               when E : others =>
                  Log_Test_Result (False, "VERIFY_AIRPORT_COUNT error: " &
                                  Ada.Exceptions.Exception_Message (E));
            end;

         elsif Command = "VERIFY_CONTROLLER_COUNT" and Natural (Tokens.Length) = 2 then
            begin
               Execute_Verify_Count_Test 
                 ("CONTROLLER", 
                  Trimmed_Natural (To_String (Tokens.Element (1))));
            exception
               when E : others =>
                  Log_Test_Result (False, "VERIFY_CONTROLLER_COUNT error: " &
                                  Ada.Exceptions.Exception_Message (E));
            end;

         elsif Command = "VERIFY_FLIGHT_COUNT" and Natural (Tokens.Length) = 2 then
            begin
               Execute_Verify_Count_Test 
                 ("FLIGHT", 
                  Trimmed_Natural (To_String (Tokens.Element (1))));
            exception
               when E : others =>
                  Log_Test_Result (False, "VERIFY_FLIGHT_COUNT error: " &
                                  Ada.Exceptions.Exception_Message (E));
            end;

         else
            Log_Test_Result (False, "Unknown command '" & Command & 
                            "' or wrong arg count (got " & 
                            Natural (Tokens.Length)'Image & ")");
         end if;
      end;
   end Process_Command_Vector;

   function Parse_Test_Line
     (Line : String; Length : Natural) return Boolean is
      Working_Line : constant String := 
        Line (Line'First .. Line'First + Length - 1);
      Tokens : constant String_Vectors.Vector := 
        Split_Line_To_Vector (Working_Line);
   begin
      Process_Command_Vector (Tokens);
      return True;
   exception
      when E : others =>
         Log_Test_Result (False, "Parse error: " & 
                          Ada.Exceptions.Exception_Message (E));
         return False;
   end Parse_Test_Line;

   procedure Show_Test_Results
     (Total_Tests, Passed_Tests : Natural) is
   begin
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Put_Line ("=====================================");
      Ada.Text_IO.Put_Line ("📊 TEST RESULTS SUMMARY:");
      Ada.Text_IO.Put_Line ("Total Tests: " & Natural'Image (Total_Tests));
      Ada.Text_IO.Put_Line ("Passed: " & Natural'Image (Passed_Tests));
      Ada.Text_IO.Put_Line ("Failed: " & 
                            Natural'Image (Total_Tests - Passed_Tests));

      if Passed_Tests = Total_Tests then
         Ada.Text_IO.Put_Line ("🎉 ALL TESTS PASSED!");
      else
         Ada.Text_IO.Put_Line ("⚠️  SOME TESTS FAILED");
      end if;
      Ada.Text_IO.Put_Line ("=====================================");
   end Show_Test_Results;

   procedure Handle_Run_Test_Cases is
      Test_File : Ada.Text_IO.File_Type;
      Line : String (1 .. 200);
      Line_Length : Natural;
   begin
      Ada.Text_IO.Put_Line ("🧪 Running Test Cases from file...");
      Ada.Text_IO.Put_Line ("=====================================");

      Test_Count := 0;
      Pass_Count := 0;

      if not Ada.Directories.Exists ("test_cases.txt") then
         Ada.Text_IO.Put_Line ("❌ Test file 'test_cases.txt' not found!");
         Ada.Text_IO.Put_Line 
           ("Please create the test file in project root.");
         return;
      end if;

      Clear_All_Data;
      Ada.Text_IO.Open (Test_File, Ada.Text_IO.In_File, "test_cases.txt");

      while not Ada.Text_IO.End_Of_File (Test_File) loop
         Ada.Text_IO.Get_Line (Test_File, Line, Line_Length);

         if Line_Length > 0 then
            declare
               Test_Success : Boolean;
               pragma Unreferenced (Test_Success);
            begin
               Test_Success := Parse_Test_Line (Line, Line_Length);
            end;
         end if;
      end loop;

      Ada.Text_IO.Close (Test_File);
      Show_Test_Results (Test_Count, Pass_Count);
      Save_All_Data;

   exception
      when others =>
         if Ada.Text_IO.Is_Open (Test_File) then
            Ada.Text_IO.Close (Test_File);
         end if;
         Ada.Text_IO.Put_Line ("💥 Error occurred during test execution");
   end Handle_Run_Test_Cases;

end Test_Runner;
