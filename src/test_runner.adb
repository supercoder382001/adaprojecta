pragma Ada_2012;
with Ada.Text_IO;
with Ada.Directories;
with Database_Operations; use Database_Operations;

package body Test_Runner is

   --  Global test counters
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

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
         Log_Test_Result (False, Entity_Type & " not found: "
                          & Identifier);
      when others =>
         Log_Test_Result (False, "Error deleting " & Entity_Type
                          & ": " & Identifier);
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
         Log_Test_Result (False, "Unknown entity type for count: "
                          & Entity_Type);
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

   function Parse_Test_Line
     (Line : String; Length : Natural) return Boolean is
      Tokens : array (1 .. 10) of String (1 .. 50);
      Token_Lengths : array (1 .. 10) of Natural := (others => 0);
      Token_Count : Natural := 0;
      I : Natural := 1;
      Start_Pos : Natural;
      Clean_Line : String (1 .. Length);
      Clean_Length : Natural := 0;

   begin
      --  First, clean the line by removing comments
      for J in 1 .. Length loop
         if Line (Line'First + J - 1) = '#' then
            exit;
         end if;
         Clean_Line (J) := Line (Line'First + J - 1);
         Clean_Length := J;
      end loop;

      --  Skip leading spaces
      while I <= Clean_Length and then
            (Clean_Line (I) = ' ' or Clean_Line (I) = ASCII.HT) loop
         I := I + 1;
      end loop;

      --  Extract tokens
      while I <= Clean_Length loop
         Start_Pos := I;

         --  Find end of current token (non-space character)
         while I <= Clean_Length and then
               Clean_Line (I) /= ' ' and Clean_Line (I) /= ASCII.HT loop
            I := I + 1;
         end loop;

         --  Store the token
         if I > Start_Pos then
            Token_Count := Token_Count + 1;
            Token_Lengths (Token_Count) := I - Start_Pos;
            Tokens (Token_Count) := (others => ' ');
            Tokens (Token_Count) (1 .. Token_Lengths (Token_Count)) :=
              Clean_Line (Start_Pos .. I - 1);
         end if;

         --  Skip spaces
         while I <= Clean_Length and then
               (Clean_Line (I) = ' ' or Clean_Line (I) = ASCII.HT) loop
            I := I + 1;
         end loop;
      end loop;

      --  Skip empty lines
      if Token_Count = 0 then
         return True;
      end if;

      --  Process commands
      declare
         Command : constant String := Tokens (1) (1 .. Token_Lengths (1));
      begin
         if Command = "ADD_AIRPORT" and Token_Count = 4 then
            Execute_Add_Airport_Test
              (Tokens (2) (1 .. Token_Lengths (2)),
               Tokens (3) (1 .. Token_Lengths (3)),
               Positive'Value (Tokens (4) (1 .. Token_Lengths (4))));

         elsif Command = "ADD_CONTROLLER" and Token_Count = 4 then
            Execute_Add_Controller_Test
              (Tokens (2) (1 .. Token_Lengths (2)),
               Tokens (3) (1 .. Token_Lengths (3)),
               Natural'Value (Tokens (4) (1 .. Token_Lengths (4))));

         elsif Command = "ADD_FLIGHT" and Token_Count = 4 then
            Execute_Add_Flight_Test
              (Tokens (2) (1 .. Token_Lengths (2)),
               Tokens (3) (1 .. Token_Lengths (3)),
               Tokens (4) (1 .. Token_Lengths (4)));

         elsif Command = "DELETE_AIRPORT" and Token_Count = 2 then
            Execute_Delete_Test ("AIRPORT",
                                 Tokens (2) (1 .. Token_Lengths (2)));

         elsif Command = "DELETE_CONTROLLER" and Token_Count = 2 then
            Execute_Delete_Test ("CONTROLLER",
                                 Tokens (2) (1 .. Token_Lengths (2)));

         elsif Command = "DELETE_FLIGHT" and Token_Count = 2 then
            Execute_Delete_Test ("FLIGHT",
                                 Tokens (2) (1 .. Token_Lengths (2)));

         elsif Command = "VERIFY_AIRPORT_COUNT" and Token_Count = 2 then
            Execute_Verify_Count_Test
              ("AIRPORT", 
               Natural'Value (Tokens (2) (1 .. Token_Lengths (2))));

         elsif Command = "VERIFY_CONTROLLER_COUNT" and Token_Count = 2 then
            Execute_Verify_Count_Test
              ("CONTROLLER", 
               Natural'Value (Tokens (2) (1 .. Token_Lengths (2))));

         elsif Command = "VERIFY_FLIGHT_COUNT" and Token_Count = 2 then
            Execute_Verify_Count_Test
              ("FLIGHT", 
               Natural'Value (Tokens (2) (1 .. Token_Lengths (2))));

         else
            Log_Test_Result (False, "Unknown command '" & Command &
                           "' or wrong arg count (got" & 
                           Token_Count'Image & ")");
         end if;
      end;

      return True;
   exception
      when others =>
         Log_Test_Result (False, "Parse error on line: " &
                          Line (Line'First .. Line'First + Length - 1));
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

      --  Reset counters
      Test_Count := 0;
      Pass_Count := 0;

      --  Check if test file exists
      if not Ada.Directories.Exists ("test_cases.txt") then
         Ada.Text_IO.Put_Line ("❌ Test file 'test_cases.txt' not found!");
         Ada.Text_IO.Put_Line
           ("Please create the test file in project root.");
         return;
      end if;

      --  Clear existing data before tests
      Clear_All_Data;

      --  Read and execute test cases
      Ada.Text_IO.Open (Test_File, Ada.Text_IO.In_File, "test_cases.txt");

      while not Ada.Text_IO.End_Of_File (Test_File) loop
         Ada.Text_IO.Get_Line (Test_File, Line, Line_Length);

         --  Skip empty lines and comments
         if Line_Length > 0 and then Line (Line'First) /= '#' then
            declare
               Test_Success : Boolean;
               pragma Unreferenced (Test_Success);
            begin
               Test_Success := Parse_Test_Line (Line, Line_Length);
            end;
         end if;
      end loop;

      Ada.Text_IO.Close (Test_File);

      --  Show results summary
      Show_Test_Results (Test_Count, Pass_Count);

      --  Save test results
      Save_All_Data;

   exception
      when others =>
         if Ada.Text_IO.Is_Open (Test_File) then
            Ada.Text_IO.Close (Test_File);
         end if;
         Ada.Text_IO.Put_Line ("💥 Error occurred during test execution");
   end Handle_Run_Test_Cases;

end Test_Runner;
