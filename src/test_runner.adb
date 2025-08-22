pragma Ada_2012;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Database_Operations; use Database_Operations;
with Flight_Types; use Flight_Types;

package body Test_Runner is

   -- Global test counters
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Log_Test_Result(Success : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Success then
         Pass_Count := Pass_Count + 1;
         Ada.Text_IO.Put_Line("✓ PASS: " & Message);
      else
         Ada.Text_IO.Put_Line("✗ FAIL: " & Message);
      end if;
   end Log_Test_Result;

   procedure Execute_Add_Airport_Test(Name, Location : String; Capacity : Positive) is
   begin
      Add_Airport(Name, Location, Capacity);
      Log_Test_Result(True, "Added airport " & Name);
   exception
      when Duplicate_Record =>
         Log_Test_Result(False, "Duplicate airport " & Name);
      when others =>
         Log_Test_Result(False, "Error adding airport " & Name);
   end Execute_Add_Airport_Test;

   procedure Execute_Add_Controller_Test(License, Name : String; Experience : Natural) is
   begin
      Add_Controller(License, Name, Experience);
      Log_Test_Result(True, "Added controller " & License);
   exception
      when Duplicate_Record =>
         Log_Test_Result(False, "Duplicate controller " & License);
      when others =>
         Log_Test_Result(False, "Error adding controller " & License);
   end Execute_Add_Controller_Test;

   procedure Execute_Add_Flight_Test(Identifier, Origin, Destination : String) is
   begin
      Add_Flight(Identifier, Origin, Destination);
      Log_Test_Result(True, "Added flight " & Identifier);
   exception
      when Duplicate_Record =>
         Log_Test_Result(False, "Duplicate flight " & Identifier);
      when others =>
         Log_Test_Result(False, "Error adding flight " & Identifier);
   end Execute_Add_Flight_Test;

   procedure Execute_Delete_Test(Entity_Type, Identifier : String) is
   begin
      if Entity_Type = "AIRPORT" then
         Delete_Airport(Identifier);
         Log_Test_Result(True, "Deleted airport " & Identifier);
      elsif Entity_Type = "CONTROLLER" then
         Delete_Controller(Identifier);
         Log_Test_Result(True, "Deleted controller " & Identifier);
      elsif Entity_Type = "FLIGHT" then
         Delete_Flight(Identifier);
         Log_Test_Result(True, "Deleted flight " & Identifier);
      else
         Log_Test_Result(False, "Unknown entity type " & Entity_Type);
      end if;
   exception
      when Record_Not_Found =>
         Log_Test_Result(False, Entity_Type & " not found: " & Identifier);
      when others =>
         Log_Test_Result(False, "Error deleting " & Entity_Type & ": " & Identifier);
   end Execute_Delete_Test;

   procedure Execute_Verify_Count_Test(Entity_Type : String; Expected_Count : Natural) is
      Actual_Count : Natural;
   begin
      if Entity_Type = "AIRPORT" then
         Actual_Count := Natural(List_Airports.Length);
      elsif Entity_Type = "CONTROLLER" then
         Actual_Count := Natural(List_Controllers.Length);
      elsif Entity_Type = "FLIGHT" then
         Actual_Count := Natural(List_Flights.Length);
      else
         Log_Test_Result(False, "Unknown entity type for count: " & Entity_Type);
         return;
      end if;

      if Expected_Count = Actual_Count then
         Log_Test_Result(True, Entity_Type & " count correct (" & 
                        Expected_Count'Image & ")");
      else
         Log_Test_Result(False, Entity_Type & " count - expected " & 
                        Expected_Count'Image & ", got " & Actual_Count'Image);
      end if;
   end Execute_Verify_Count_Test;

   function Parse_Test_Line(Line : String; Length : Natural) return Boolean is
      Space1, Space2, Space3 : Natural := 0;
      Command : String(1 .. 50);
      Arg1 : String(1 .. 50);
      Arg2 : String(1 .. 50);
      Arg3 : String(1 .. 50);
      Command_Length, Arg1_Length, Arg2_Length, Arg3_Length : Natural := 0;
   begin
      -- Find space positions
      for I in 1 .. Length loop
         if Line(I) = ' ' then
            if Space1 = 0 then
               Space1 := I;
            elsif Space2 = 0 then
               Space2 := I;
            elsif Space3 = 0 then
               Space3 := I;
               exit;
            end if;
         end if;
      end loop;

      -- Extract command
      if Space1 > 0 then
         Command_Length := Space1 - 1;
         Command(1 .. Command_Length) := Line(1 .. Space1 - 1);
      else
         Command_Length := Length;
         Command(1 .. Command_Length) := Line(1 .. Length);
      end if;

      -- Extract arguments
      if Space2 > 0 then
         Arg1_Length := Space2 - Space1 - 1;
         Arg1(1 .. Arg1_Length) := Line(Space1 + 1 .. Space2 - 1);
      elsif Space1 > 0 then
         Arg1_Length := Length - Space1;
         Arg1(1 .. Arg1_Length) := Line(Space1 + 1 .. Length);
      end if;

      if Space3 > 0 then
         Arg2_Length := Space3 - Space2 - 1;
         Arg2(1 .. Arg2_Length) := Line(Space2 + 1 .. Space3 - 1);
         Arg3_Length := Length - Space3;
         Arg3(1 .. Arg3_Length) := Line(Space3 + 1 .. Length);
      elsif Space2 > 0 then
         Arg2_Length := Length - Space2;
         Arg2(1 .. Arg2_Length) := Line(Space2 + 1 .. Length);
      end if;

      -- Execute commands based on parsed data
      declare
         Test_Command : String := Command(1 .. Command_Length);
      begin
         if Test_Command = "ADD_AIRPORT" then
            declare
               Capacity : Positive := Positive'Value(Arg3(1 .. Arg3_Length));
            begin
               Execute_Add_Airport_Test(Arg1(1 .. Arg1_Length),
                                       Arg2(1 .. Arg2_Length),
                                       Capacity);
            end;

         elsif Test_Command = "ADD_CONTROLLER" then
            declare
               Experience : Natural := Natural'Value(Arg3(1 .. Arg3_Length));
            begin
               Execute_Add_Controller_Test(Arg1(1 .. Arg1_Length),
                                         Arg2(1 .. Arg2_Length),
                                         Experience);
            end;

         elsif Test_Command = "ADD_FLIGHT" then
            Execute_Add_Flight_Test(Arg1(1 .. Arg1_Length),
                                  Arg2(1 .. Arg2_Length),
                                  Arg3(1 .. Arg3_Length));

         elsif Test_Command = "DELETE_AIRPORT" then
            Execute_Delete_Test("AIRPORT", Arg1(1 .. Arg1_Length));

         elsif Test_Command = "DELETE_CONTROLLER" then
            Execute_Delete_Test("CONTROLLER", Arg1(1 .. Arg1_Length));

         elsif Test_Command = "DELETE_FLIGHT" then
            Execute_Delete_Test("FLIGHT", Arg1(1 .. Arg1_Length));

         elsif Test_Command = "VERIFY_AIRPORT_COUNT" then
            declare
               Expected : Natural := Natural'Value(Arg1(1 .. Arg1_Length));
            begin
               Execute_Verify_Count_Test("AIRPORT", Expected);
            end;

         elsif Test_Command = "VERIFY_CONTROLLER_COUNT" then
            declare
               Expected : Natural := Natural'Value(Arg1(1 .. Arg1_Length));
            begin
               Execute_Verify_Count_Test("CONTROLLER", Expected);
            end;

         elsif Test_Command = "VERIFY_FLIGHT_COUNT" then
            declare
               Expected : Natural := Natural'Value(Arg1(1 .. Arg1_Length));
            begin
               Execute_Verify_Count_Test("FLIGHT", Expected);
            end;

         else
            Log_Test_Result(False, "Unknown command: " & Test_Command);
         end if;
      end;

      return True;
   exception
      when others =>
         Log_Test_Result(False, "Error parsing test line: " & Line(1 .. Length));
         return False;
   end Parse_Test_Line;

   procedure Show_Test_Results(Total_Tests, Passed_Tests : Natural) is
   begin
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("=====================================");
      Ada.Text_IO.Put_Line("📊 TEST RESULTS SUMMARY:");
      Ada.Text_IO.Put_Line("Total Tests: " & Natural'Image(Total_Tests));
      Ada.Text_IO.Put_Line("Passed: " & Natural'Image(Passed_Tests));
      Ada.Text_IO.Put_Line("Failed: " & Natural'Image(Total_Tests - Passed_Tests));

      if Passed_Tests = Total_Tests then
         Ada.Text_IO.Put_Line("🎉 ALL TESTS PASSED!");
      else
         Ada.Text_IO.Put_Line("⚠️  SOME TESTS FAILED");
      end if;
      Ada.Text_IO.Put_Line("=====================================");
   end Show_Test_Results;

   procedure Handle_Run_Test_Cases is
      Test_File : Ada.Text_IO.File_Type;
      Line : String(1 .. 200);
      Line_Length : Natural;
   begin
      Ada.Text_IO.Put_Line("🧪 Running Test Cases from file...");
      Ada.Text_IO.Put_Line("=====================================");

      -- Reset counters
      Test_Count := 0;
      Pass_Count := 0;

      -- Check if test file exists
      if not Ada.Directories.Exists("test_cases.txt") then
         Ada.Text_IO.Put_Line("❌ Test file 'test_cases.txt' not found!");
         Ada.Text_IO.Put_Line("Please create the test file in project root.");
         return;
      end if;

      -- Clear existing data before tests
      Clear_All_Data;

      -- Read and execute test cases
      Ada.Text_IO.Open(Test_File, Ada.Text_IO.In_File, "test_cases.txt");

      while not Ada.Text_IO.End_Of_File(Test_File) loop
         Ada.Text_IO.Get_Line(Test_File, Line, Line_Length);
         
         -- Skip empty lines and comments
         if Line_Length > 0 and then Line(1) /= '#' then
            declare
               Success : Boolean := Parse_Test_Line(Line, Line_Length);
            begin
               null; -- Result already logged by Parse_Test_Line
            end;
         end if;
      end loop;

      Ada.Text_IO.Close(Test_File);

      -- Show results summary
      Show_Test_Results(Test_Count, Pass_Count);

      -- Save test results
      Save_All_Data;

   exception
      when others =>
         if Ada.Text_IO.Is_Open(Test_File) then
            Ada.Text_IO.Close(Test_File);
         end if;
         Ada.Text_IO.Put_Line("💥 Error occurred during test execution");
   end Handle_Run_Test_Cases;

end Test_Runner;
