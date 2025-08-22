pragma Ada_2012;
with Ada.Text_IO;
with Database_Operations; use Database_Operations;
with Flight_Types; use Flight_Types;
with Sync_Operations; use Sync_Operations;
with Test_Runner;  -- NEW IMPORT

procedure Main is

   procedure Show_Menu;
   procedure Handle_Add_Airport;
   procedure Handle_List_Airports;
   procedure Handle_Update_Airport;
   procedure Handle_Delete_Airport;
   procedure Handle_Add_Controller;
   procedure Handle_List_Controllers;
   procedure Handle_Update_Controller;
   procedure Handle_Delete_Controller;
   procedure Handle_Add_Flight;
   procedure Handle_List_Flights;
   procedure Handle_Update_Flight;
   procedure Handle_Delete_Flight;
   procedure Handle_Advanced_Options;

   Choice_Input  : String (1 .. 10);
   Choice_Length : Natural;
   Running       : Boolean := True;

   procedure Show_Menu is
   begin
      Ada.Text_IO.Put_Line ("=== Flight Management System ===");
      Ada.Text_IO.Put_Line ("Database: PostgreSQL with UUID Schema");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("AIRPORTS:");
      Ada.Text_IO.Put_Line ("[1] Add Airport      [2] List Airports");
      Ada.Text_IO.Put_Line (" Update Airport    Delete Airport");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("CONTROLLERS:");
      Ada.Text_IO.Put_Line ("[5] Add Controller   [6] List Controllers");
      Ada.Text_IO.Put_Line (" Update Controller  Delete Controller");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("FLIGHTS:");
      Ada.Text_IO.Put_Line ("[9] Add Flight       [A] List Flights");
      Ada.Text_IO.Put_Line ("[B] Update Flight    [D] Delete Flight");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("SYSTEM:");
      Ada.Text_IO.Put_Line ("[C] Clear All Data   [S] Statistics");
      Ada.Text_IO.Put_Line ("[T] Run Test Cases");
      Ada.Text_IO.Put_Line ("[E] Export to JSON   [X] Backup Data");
      Ada.Text_IO.Put_Line ("[M] Advanced Options [Q] Quit");
      Ada.Text_IO.New_Line;
   end Show_Menu;

   procedure Handle_Add_Airport is
      Name_Input      : String (1 .. 50);
      Name_Length     : Natural;
      Location_Input  : String (1 .. 100);
      Location_Length : Natural;
      Capacity_Input  : String (1 .. 10);
      Capacity_Length : Natural;
      Capacity_Value  : Positive;
      Valid_Input     : Boolean := False;
   begin
      Ada.Text_IO.Put_Line ("=== Add Airport ===");

      Ada.Text_IO.Put ("Airport Name (max 50 chars): ");
      Ada.Text_IO.Get_Line (Name_Input, Name_Length);

      if Name_Length = 0 or Name_Length > 50 then
         Ada.Text_IO.Put_Line ("ERROR: Airport name must be " &
                               "1-50 characters");
         return;
      end if;

      Ada.Text_IO.Put ("Location (max 100 chars): ");
      Ada.Text_IO.Get_Line (Location_Input, Location_Length);

      if Location_Length = 0 or Location_Length > 100 then
         Ada.Text_IO.Put_Line ("ERROR: Location must be 1-100 characters");
         return;
      end if;

      while not Valid_Input loop
         Ada.Text_IO.Put ("Max Capacity (positive integer): ");
         Ada.Text_IO.Get_Line (Capacity_Input, Capacity_Length);

         if Capacity_Length > 0 then
            declare
               Tmp_Str  : String := Capacity_Input (1 .. Capacity_Length);
               Is_Valid : Boolean := True;
            begin
               for I in Tmp_Str'Range loop
                  if not (Tmp_Str (I) in '0' .. '9') then
                     Is_Valid := False;
                     exit;
                  end if;
               end loop;

               if Is_Valid then
                  Capacity_Value := Positive'Value (Tmp_Str);
                  Valid_Input := True;
               else
                  Ada.Text_IO.Put_Line ("ERROR: Please enter only " &
                                        "positive numbers");
               end if;
            exception
               when others =>
                  Ada.Text_IO.Put_Line ("ERROR: Invalid number format");
            end;
         else
            Ada.Text_IO.Put_Line ("ERROR: Capacity cannot be empty");
         end if;
      end loop;

      Add_Airport (Name_Input (1 .. Name_Length),
                   Location_Input (1 .. Location_Length),
                   Capacity_Value);
      Save_All_Data;
      Ada.Text_IO.Put_Line ("Airport added successfully!");
      Ada.Text_IO.Put_Line ("Saved to both file and PostgreSQL database.");
   exception
      when Duplicate_Record =>
         Ada.Text_IO.Put_Line ("ERROR: Airport name already exists!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to add airport!");
   end Handle_Add_Airport;

   procedure Handle_List_Airports is
      Airports : constant Airport_Vectors.Vector := List_Airports;
   begin
      Ada.Text_IO.Put_Line ("=== Airports List ===");
      if Airports.Is_Empty then
         Ada.Text_IO.Put_Line ("No airports found.");
         Ada.Text_IO.Put_Line ("Tip: Use [1] to add your first airport.");
      else
         Ada.Text_IO.Put_Line ("Found" & Airports.Length'Image &
                               " airport(s):");
         Ada.Text_IO.New_Line;
         for Airport of Airports loop
            Ada.Text_IO.Put_Line ("• " & Image (Airport));
         end loop;
      end if;
   end Handle_List_Airports;

   procedure Handle_Update_Airport is
      Old_Name_Input        : String (1 .. 50);
      Old_Name_Length       : Natural;
      New_Name_Input        : String (1 .. 50);
      New_Name_Length       : Natural;
      New_Location_Input    : String (1 .. 100);
      New_Location_Length   : Natural;
      New_Capacity_Input    : String (1 .. 10);
      New_Capacity_Length   : Natural;
      New_Capacity_Value    : Positive;
      Valid_Input           : Boolean := False;
   begin
      Ada.Text_IO.Put_Line ("=== Update Airport ===");

      Ada.Text_IO.Put ("Enter current airport name: ");
      Ada.Text_IO.Get_Line (Old_Name_Input, Old_Name_Length);

      if Old_Name_Length = 0 then
         Ada.Text_IO.Put_Line ("ERROR: Airport name cannot be empty");
         return;
      end if;

      Ada.Text_IO.Put ("Enter new airport name: ");
      Ada.Text_IO.Get_Line (New_Name_Input, New_Name_Length);

      Ada.Text_IO.Put ("Enter new location: ");
      Ada.Text_IO.Get_Line (New_Location_Input, New_Location_Length);

      while not Valid_Input loop
         Ada.Text_IO.Put ("Enter new capacity: ");
         Ada.Text_IO.Get_Line (New_Capacity_Input, New_Capacity_Length);

         if New_Capacity_Length > 0 then
            declare
               Tmp_Str  : String := New_Capacity_Input
                                    (1 .. New_Capacity_Length);
               Is_Valid : Boolean := True;
            begin
               for I in Tmp_Str'Range loop
                  if not (Tmp_Str (I) in '0' .. '9') then
                     Is_Valid := False;
                     exit;
                  end if;
               end loop;

               if Is_Valid then
                  New_Capacity_Value := Positive'Value (Tmp_Str);
                  Valid_Input := True;
               else
                  Ada.Text_IO.Put_Line ("ERROR: Please enter only " &
                                        "positive numbers");
               end if;
            exception
               when others =>
                  Ada.Text_IO.Put_Line ("ERROR: Invalid number format");
            end;
         else
            Ada.Text_IO.Put_Line ("ERROR: Capacity cannot be empty");
         end if;
      end loop;

      Update_Airport (Old_Name_Input (1 .. Old_Name_Length),
                      New_Name_Input (1 .. New_Name_Length),
                      New_Location_Input (1 .. New_Location_Length),
                      New_Capacity_Value);
      Save_All_Data;
      Ada.Text_IO.Put_Line ("Airport updated successfully!");
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Airport not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to update airport!");
   end Handle_Update_Airport;

   procedure Handle_Delete_Airport is
      Name_Input     : String (1 .. 50);
      Name_Length    : Natural;
      Confirm        : String (1 .. 10);
      Confirm_Length : Natural;
   begin
      Ada.Text_IO.Put_Line ("=== Delete Airport ===");

      Ada.Text_IO.Put ("Enter airport name to delete: ");
      Ada.Text_IO.Get_Line (Name_Input, Name_Length);

      if Name_Length = 0 then
         Ada.Text_IO.Put_Line ("ERROR: Airport name cannot be empty");
         return;
      end if;

      Ada.Text_IO.Put ("Are you sure you want to delete '" &
                       Name_Input (1 .. Name_Length) & "'? (yes/no): ");
      Ada.Text_IO.Get_Line (Confirm, Confirm_Length);

      if Confirm_Length >= 3 and then
         (Confirm (1 .. 3) = "yes" or Confirm (1 .. 3) = "YES")
      then
         Delete_Airport (Name_Input (1 .. Name_Length));
         Save_All_Data;
         Ada.Text_IO.Put_Line ("Airport deleted successfully!");
         Ada.Text_IO.Put_Line ("Note: Any flights using this airport " &
                               "were also removed.");
      else
         Ada.Text_IO.Put_Line ("Delete cancelled.");
      end if;
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Airport not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to delete airport!");
   end Handle_Delete_Airport;

   procedure Handle_Add_Controller is
      License_Input     : String (1 .. 20);
      License_Length    : Natural;
      Name_Input        : String (1 .. 50);
      Name_Length       : Natural;
      Experience_Input  : String (1 .. 10);
      Experience_Length : Natural;
      Experience_Value  : Natural;
      Valid_Input       : Boolean := False;
   begin
      Ada.Text_IO.Put_Line ("=== Add Controller ===");

      Ada.Text_IO.Put ("License Number (max 20 chars): ");
      Ada.Text_IO.Get_Line (License_Input, License_Length);

      if License_Length = 0 or License_Length > 20 then
         Ada.Text_IO.Put_Line ("ERROR: License must be 1-20 characters");
         return;
      end if;

      Ada.Text_IO.Put ("Controller Name (max 50 chars): ");
      Ada.Text_IO.Get_Line (Name_Input, Name_Length);

      if Name_Length = 0 or Name_Length > 50 then
         Ada.Text_IO.Put_Line ("ERROR: Name must be 1-50 characters");
         return;
      end if;

      while not Valid_Input loop
         Ada.Text_IO.Put ("Experience Years (0-50): ");
         Ada.Text_IO.Get_Line (Experience_Input, Experience_Length);

         if Experience_Length > 0 then
            declare
               Tmp_Str  : String := Experience_Input
                                    (1 .. Experience_Length);
               Is_Valid : Boolean := True;
            begin
               for I in Tmp_Str'Range loop
                  if not (Tmp_Str (I) in '0' .. '9') then
                     Is_Valid := False;
                     exit;
                  end if;
               end loop;

               if Is_Valid then
                  Experience_Value := Natural'Value (Tmp_Str);
                  if Experience_Value <= 50 then
                     Valid_Input := True;
                  else
                     Ada.Text_IO.Put_Line ("ERROR: Experience should be " &
                                           "0-50 years");
                  end if;
               else
                  Ada.Text_IO.Put_Line ("ERROR: Please enter only numbers");
               end if;
            exception
               when others =>
                  Ada.Text_IO.Put_Line ("ERROR: Invalid number format");
            end;
         else
            Ada.Text_IO.Put_Line ("ERROR: Experience cannot be empty");
         end if;
      end loop;

      Add_Controller (License_Input (1 .. License_Length),
                      Name_Input (1 .. Name_Length),
                      Experience_Value);
      Save_All_Data;
      Ada.Text_IO.Put_Line ("Controller added successfully!");
   exception
      when Duplicate_Record =>
         Ada.Text_IO.Put_Line ("ERROR: License number already exists!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to add controller!");
   end Handle_Add_Controller;

   procedure Handle_List_Controllers is
      Controllers : constant Controller_Vectors.Vector := List_Controllers;
   begin
      Ada.Text_IO.Put_Line ("=== Controllers List ===");
      if Controllers.Is_Empty then
         Ada.Text_IO.Put_Line ("No controllers found.");
         Ada.Text_IO.Put_Line ("Tip: Use [5] to add your first controller.");
      else
         Ada.Text_IO.Put_Line ("Found" & Controllers.Length'Image &
                               " controller(s):");
         Ada.Text_IO.New_Line;
         for Controller of Controllers loop
            Ada.Text_IO.Put_Line ("• " & Image (Controller));
         end loop;
      end if;
   end Handle_List_Controllers;

   procedure Handle_Update_Controller is
      Old_License_Input     : String (1 .. 20);
      Old_License_Length    : Natural;
      New_Name_Input        : String (1 .. 50);
      New_Name_Length       : Natural;
      New_Experience_Input  : String (1 .. 10);
      New_Experience_Length : Natural;
      New_Experience_Value  : Natural;
      Valid_Input           : Boolean := False;
   begin
      Ada.Text_IO.Put_Line ("=== Update Controller ===");

      Ada.Text_IO.Put ("Enter current license number: ");
      Ada.Text_IO.Get_Line (Old_License_Input, Old_License_Length);

      Ada.Text_IO.Put ("Enter new name: ");
      Ada.Text_IO.Get_Line (New_Name_Input, New_Name_Length);

      while not Valid_Input loop
         Ada.Text_IO.Put ("Enter new experience years: ");
         Ada.Text_IO.Get_Line (New_Experience_Input, New_Experience_Length);

         if New_Experience_Length > 0 then
            declare
               Tmp_Str : String := New_Experience_Input
                                   (1 .. New_Experience_Length);
               Is_Valid : Boolean := True;
            begin
               for I in Tmp_Str'Range loop
                  if not (Tmp_Str (I) in '0' .. '9') then
                     Is_Valid := False;
                     exit;
                  end if;
               end loop;

               if Is_Valid then
                  New_Experience_Value := Natural'Value (Tmp_Str);
                  Valid_Input := True;
               else
                  Ada.Text_IO.Put_Line ("ERROR: Please enter only numbers");
               end if;
            exception
               when others =>
                  Ada.Text_IO.Put_Line ("ERROR: Invalid number format");
            end;
         else
            Ada.Text_IO.Put_Line ("ERROR: Experience cannot be empty");
         end if;
      end loop;

      Update_Controller (Old_License_Input (1 .. Old_License_Length),
                         New_Name_Input (1 .. New_Name_Length),
                         New_Experience_Value);
      Save_All_Data;
      Ada.Text_IO.Put_Line ("Controller updated successfully!");
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Controller not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to update controller!");
   end Handle_Update_Controller;

   procedure Handle_Delete_Controller is
      License_Input  : String (1 .. 20);
      License_Length : Natural;
      Confirm        : String (1 .. 10);
      Confirm_Length : Natural;
   begin
      Ada.Text_IO.Put_Line ("=== Delete Controller ===");

      Ada.Text_IO.Put ("Enter license number to delete: ");
      Ada.Text_IO.Get_Line (License_Input, License_Length);

      Ada.Text_IO.Put ("Are you sure? (yes/no): ");
      Ada.Text_IO.Get_Line (Confirm, Confirm_Length);

      if Confirm_Length >= 3 and then
         (Confirm (1 .. 3) = "yes" or Confirm (1 .. 3) = "YES")
      then
         Delete_Controller (License_Input (1 .. License_Length));
         Save_All_Data;
         Ada.Text_IO.Put_Line ("Controller deleted successfully!");
      else
         Ada.Text_IO.Put_Line ("Delete cancelled.");
      end if;
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Controller not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to delete controller!");
   end Handle_Delete_Controller;

   procedure Handle_Add_Flight is
      Identifier_Input  : String (1 .. 10);
      Identifier_Length : Natural;
      Origin_Input      : String (1 .. 50);
      Origin_Length     : Natural;
      Dest_Input        : String (1 .. 50);
      Dest_Length       : Natural;
   begin
      Ada.Text_IO.Put_Line ("=== Add Flight ===");

      Ada.Text_IO.Put ("Flight Identifier (max 10 chars): ");
      Ada.Text_IO.Get_Line (Identifier_Input, Identifier_Length);

      if Identifier_Length = 0 or Identifier_Length > 10 then
         Ada.Text_IO.Put_Line ("ERROR: Flight identifier must be " &
                               "1-10 characters");
         return;
      end if;

      Ada.Text_IO.Put ("Origin Airport Name: ");
      Ada.Text_IO.Get_Line (Origin_Input, Origin_Length);

      if Origin_Length = 0 then
         Ada.Text_IO.Put_Line ("ERROR: Origin airport cannot be empty");
         return;
      end if;

      Ada.Text_IO.Put ("Destination Airport Name: ");
      Ada.Text_IO.Get_Line (Dest_Input, Dest_Length);

      if Dest_Length = 0 then
         Ada.Text_IO.Put_Line ("ERROR: Destination airport cannot be empty");
         return;
      end if;

      if Origin_Input (1 .. Origin_Length) =
         Dest_Input (1 .. Dest_Length)
      then
         Ada.Text_IO.Put_Line ("ERROR: Origin and destination cannot " &
                               "be the same");
         return;
      end if;

      Add_Flight (Identifier_Input (1 .. Identifier_Length),
                  Origin_Input (1 .. Origin_Length),
                  Dest_Input (1 .. Dest_Length));
      Save_All_Data;
      Ada.Text_IO.Put_Line ("Flight added successfully!");
      Ada.Text_IO.Put_Line ("Note: Airports must exist in the system first.");
   exception
      when Duplicate_Record =>
         Ada.Text_IO.Put_Line ("ERROR: Flight identifier already exists!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to add flight!");
         Ada.Text_IO.Put_Line ("Check if both airports exist in the system.");
   end Handle_Add_Flight;

   procedure Handle_List_Flights is
      Flights : constant Flight_Vectors.Vector := List_Flights;
   begin
      Ada.Text_IO.Put_Line ("=== Flights List ===");
      if Flights.Is_Empty then
         Ada.Text_IO.Put_Line ("No flights found.");
         Ada.Text_IO.Put_Line ("Tip: Add airports first, " &
                               "then use [9] to add flights.");
      else
         Ada.Text_IO.Put_Line ("Found" & Flights.Length'Image &
                               " flight(s):");
         Ada.Text_IO.New_Line;
         for Flight of Flights loop
            Ada.Text_IO.Put_Line ("• " & Image (Flight));
         end loop;
      end if;
   end Handle_List_Flights;

   procedure Handle_Update_Flight is
      Old_Identifier_Input  : String (1 .. 10);
      Old_Identifier_Length : Natural;
      New_Origin_Input      : String (1 .. 50);
      New_Origin_Length     : Natural;
      New_Dest_Input        : String (1 .. 50);
      New_Dest_Length       : Natural;
   begin
      Ada.Text_IO.Put_Line ("=== Update Flight ===");

      Ada.Text_IO.Put ("Enter flight identifier to update: ");
      Ada.Text_IO.Get_Line (Old_Identifier_Input, Old_Identifier_Length);

      Ada.Text_IO.Put ("Enter new origin airport: ");
      Ada.Text_IO.Get_Line (New_Origin_Input, New_Origin_Length);

      Ada.Text_IO.Put ("Enter new destination airport: ");
      Ada.Text_IO.Get_Line (New_Dest_Input, New_Dest_Length);

      if New_Origin_Input (1 .. New_Origin_Length) =
         New_Dest_Input (1 .. New_Dest_Length)
      then
         Ada.Text_IO.Put_Line ("ERROR: Origin and destination cannot " &
                               "be the same");
         return;
      end if;

      Update_Flight (Old_Identifier_Input (1 .. Old_Identifier_Length),
                     New_Origin_Input (1 .. New_Origin_Length),
                     New_Dest_Input (1 .. New_Dest_Length));
      Save_All_Data;
      Ada.Text_IO.Put_Line ("Flight updated successfully!");
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Flight not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to update flight!");
   end Handle_Update_Flight;

   procedure Handle_Delete_Flight is
      Identifier_Input  : String (1 .. 10);
      Identifier_Length : Natural;
      Confirm           : String (1 .. 10);
      Confirm_Length    : Natural;
   begin
      Ada.Text_IO.Put_Line ("=== Delete Flight ===");

      Ada.Text_IO.Put ("Enter flight identifier to delete: ");
      Ada.Text_IO.Get_Line (Identifier_Input, Identifier_Length);

      Ada.Text_IO.Put ("Are you sure? (yes/no): ");
      Ada.Text_IO.Get_Line (Confirm, Confirm_Length);

      if Confirm_Length >= 3 and then
         (Confirm (1 .. 3) = "yes" or Confirm (1 .. 3) = "YES")
      then
         Delete_Flight (Identifier_Input (1 .. Identifier_Length));
         Save_All_Data;
         Ada.Text_IO.Put_Line ("Flight deleted successfully!");
      else
         Ada.Text_IO.Put_Line ("Delete cancelled.");
      end if;
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Flight not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to delete flight!");
   end Handle_Delete_Flight;

   procedure Handle_Advanced_Options is
      Option_Input  : String (1 .. 10);
      Option_Length : Natural;
   begin
      Ada.Text_IO.Put_Line ("=== Advanced Options ===");
      Ada.Text_IO.Put_Line ("[1] Database Connection Test");
      Ada.Text_IO.Put_Line (" Force Sync File to Database");
      Ada.Text_IO.Put_Line (" View Database Schema Info");
      Ada.Text_IO.Put_Line (" Manual SQL Access Tip");
      Ada.Text_IO.Put_Line (" Back to Main Menu");
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put ("Select option: ");
      Ada.Text_IO.Get_Line (Option_Input, Option_Length);

      if Option_Length > 0 then
         case Option_Input (1) is
            when '1' =>
               Ada.Text_IO.Put_Line ("Testing database connection...");
               Get_Database_Statistics;

            when '2' =>
               Ada.Text_IO.Put_Line ("Forcing sync from file to database...");
               Load_From_File;
               Save_To_Database;
               Ada.Text_IO.Put_Line ("Sync completed!");

            when '3' =>
               Ada.Text_IO.Put_Line ("=== Database Schema Info ===");
               Ada.Text_IO.Put_Line ("Database: flight_management");
               Ada.Text_IO.Put_Line ("User: flight_user");
               Ada.Text_IO.Put_Line ("Tables: airports, controllers, flights");
               Ada.Text_IO.Put_Line ("Features: UUID keys, foreign keys, " &
                                     "triggers");
               Ada.Text_IO.Put_Line ("Stored Functions: add_*, list_*, " &
                                     "update_*, delete_*");

            when '4' =>
               Ada.Text_IO.Put_Line ("=== Manual PostgreSQL Access ===");
               Ada.Text_IO.Put_Line ("Command: psql -U flight_user " &
                                     "-d flight_management");
               Ada.Text_IO.Put_Line ("Password: flight_password");
               Ada.Text_IO.Put_Line ("");
               Ada.Text_IO.Put_Line ("Try these queries:");
               Ada.Text_IO.Put_Line ("  SELECT * FROM list_airports();");
               Ada.Text_IO.Put_Line ("  SELECT * FROM list_controllers();");
               Ada.Text_IO.Put_Line ("  SELECT * FROM list_flights();");
               Ada.Text_IO.Put_Line ("  SELECT * FROM airports;");

            when '0' =>
               return;

            when others =>
               Ada.Text_IO.Put_Line ("Invalid option.");
         end case;
      end if;
   end Handle_Advanced_Options;

begin
   Initialize_Database_Connection;
   Initialize_Sync_Config;
   Load_All_Data;

   Ada.Text_IO.Put_Line ("Welcome to Flight Management System!");
   Ada.Text_IO.Put_Line ("PostgreSQL Schema with UUID Support");
   Ada.Text_IO.New_Line;

   while Running loop
      Show_Menu;
      Ada.Text_IO.Put ("Enter your choice: ");
      Ada.Text_IO.Get_Line (Choice_Input, Choice_Length);

      if Choice_Length > 0 then
         case Choice_Input (1) is
            when '1' =>
               Handle_Add_Airport;
            when '2' =>
               Handle_List_Airports;
            when '3' =>
               Handle_Update_Airport;
            when '4' =>
               Handle_Delete_Airport;
            when '5' =>
               Handle_Add_Controller;
            when '6' =>
               Handle_List_Controllers;
            when '7' =>
               Handle_Update_Controller;
            when '8' =>
               Handle_Delete_Controller;
            when '9' =>
               Handle_Add_Flight;
            when 'A' | 'a' =>
               Handle_List_Flights;
            when 'B' | 'b' =>
               Handle_Update_Flight;
            when 'D' | 'd' =>
               Handle_Delete_Flight;
            when 'C' | 'c' =>
               declare
                  Confirm        : String (1 .. 10);
                  Confirm_Length : Natural;
               begin
                  Ada.Text_IO.Put ("Clear ALL data? This cannot be " &
                                   "undone! (yes/no): ");
                  Ada.Text_IO.Get_Line (Confirm, Confirm_Length);

                  if Confirm_Length >= 3 and then
                     (Confirm (1 .. 3) = "yes" or Confirm (1 .. 3) = "YES")
                  then
                     Clear_All_Data;
                     Ada.Text_IO.Put_Line ("All data cleared successfully!");
                  else
                     Ada.Text_IO.Put_Line ("Clear cancelled.");
                  end if;
               end;
            when 'S' | 's' =>
               Get_Database_Statistics;
            when 'E' | 'e' =>
               Export_All_To_JSON;
            when 'T' | 't' =>                    -- NEW CASE
               Test_Runner.Handle_Run_Test_Cases;  -- CALL TEST_RUNNER
               Ada.Text_IO.Put_Line ("Press Enter to continue...");
               Ada.Text_IO.Skip_Line;
            when 'X' | 'x' =>
               Create_Full_Backup;
            when 'M' | 'm' =>
               Handle_Advanced_Options;
            when 'Q' | 'q' =>
               Ada.Text_IO.Put_Line ("Saving data before exit...");
               Save_All_Data;
               Ada.Text_IO.Put_Line ("Shutting down...");
               Running := False;
            when others =>
               Ada.Text_IO.Put_Line ("Invalid choice. Please try again.");
         end case;
      else
         Ada.Text_IO.Put_Line ("Please enter a valid choice.");
      end if;

      if Running then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Press Enter to continue...");
         Ada.Text_IO.Skip_Line;
         Ada.Text_IO.Put_Line (String'(1 .. 50 => '='));
      end if;
   end loop;

   Save_All_Data;
   Shutdown_Database_Connection;
   Ada.Text_IO.Put_Line ("Goodbye! Data saved to file and database.");

exception
   when others =>
      Save_All_Data;
      Ada.Text_IO.Put_Line ("Fatal error occurred. Emergency save completed.");
      Shutdown_Database_Connection;
end Main;
