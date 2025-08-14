pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed;

with Database_Operations; use Database_Operations;
with Sync_Operations; use Sync_Operations;
with Flight_Types; use Flight_Types;

procedure Main is

   -- Forward declarations (specs) to fix style warnings
   function Get_Input (Prompt : String) return String;
   function Get_Positive_Input (Prompt : String) return Positive;
   function Get_Natural_Input (Prompt : String) return Natural;

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

   procedure Handle_Startup_Options;
   procedure Show_Menu;

   Choice : Character;

   -- Implementations
   function Get_Input (Prompt : String) return String is
   begin
      Put (Prompt);
      return Ada.Strings.Fixed.Trim (Source => Get_Line, 
                                      Side => Ada.Strings.Both);
   end Get_Input;

   function Get_Positive_Input (Prompt : String) return Positive is
   begin
      return Positive'Value (Get_Input (Prompt));
   end Get_Positive_Input;

   function Get_Natural_Input (Prompt : String) return Natural is
   begin
      return Natural'Value (Get_Input (Prompt));
   end Get_Natural_Input;

   procedure Handle_Add_Airport is
   begin
      Database_Operations.Add_Airport (
         Name => Get_Input ("Name: "),
         Location => Get_Input ("Location: "),
         Capacity => Get_Positive_Input ("Capacity: ")
      );
      Put_Line ("SUCCESS: Airport added.");
   end Handle_Add_Airport;

   procedure Handle_List_Airports is
   begin
      Put_Line ("--- Airports List ---");
      for Item of Database_Operations.List_Airports loop
         Put_Line (Flight_Types.Image (Item));
      end loop;
   end Handle_List_Airports;

   procedure Handle_Update_Airport is
   begin
      Database_Operations.Update_Airport (
         Old_Name => Get_Input ("Current Name: "),
         New_Name => Get_Input ("New Name: "),
         New_Location => Get_Input ("New Location: "),
         New_Capacity => Get_Positive_Input ("New Capacity: ")
      );
      Put_Line ("SUCCESS: Airport updated.");
   end Handle_Update_Airport;

   procedure Handle_Delete_Airport is
   begin
      Database_Operations.Delete_Airport (Get_Input ("Name to Delete: "));
      Put_Line ("SUCCESS: Airport deleted.");
   end Handle_Delete_Airport;

   procedure Handle_Add_Controller is
   begin
      Database_Operations.Add_Controller (
         License => Get_Input ("License: "),
         Name => Get_Input ("Name: "),
         Experience => Get_Natural_Input ("Experience (yrs): ")
      );
      Put_Line ("SUCCESS: Controller added.");
   end Handle_Add_Controller;

   procedure Handle_List_Controllers is
   begin
      Put_Line ("--- Controllers List ---");
      for Item of Database_Operations.List_Controllers loop
         Put_Line (Flight_Types.Image (Item));
      end loop;
   end Handle_List_Controllers;

   procedure Handle_Update_Controller is
   begin
      Database_Operations.Update_Controller (
         Old_License => Get_Input ("Current License: "),
         New_Name => Get_Input ("New Name: "),
         New_Experience => Get_Natural_Input ("New Experience (yrs): ")
      );
      Put_Line ("SUCCESS: Controller updated.");
   end Handle_Update_Controller;

   procedure Handle_Delete_Controller is
   begin
      Database_Operations.Delete_Controller (Get_Input ("License to Delete: "));
      Put_Line ("SUCCESS: Controller deleted.");
   end Handle_Delete_Controller;

   procedure Handle_Add_Flight is
   begin
      Database_Operations.Add_Flight (
         Identifier => Get_Input ("Identifier: "),
         Origin => Get_Input ("Origin Airport: "),
         Destination => Get_Input ("Destination Airport: ")
      );
      Put_Line ("SUCCESS: Flight added.");
   end Handle_Add_Flight;

   procedure Handle_List_Flights is
   begin
      Put_Line ("--- Flights List ---");
      for Item of Database_Operations.List_Flights loop
         Put_Line (Flight_Types.Image (Item));
      end loop;
   end Handle_List_Flights;

   procedure Handle_Update_Flight is
   begin
      Database_Operations.Update_Flight (
         Old_Identifier => Get_Input ("Current Identifier: "),
         New_Origin => Get_Input ("New Origin Airport: "),
         New_Destination => Get_Input ("New Destination Airport: ")
      );
      Put_Line ("SUCCESS: Flight updated.");
   end Handle_Update_Flight;

   procedure Handle_Delete_Flight is
   begin
      Database_Operations.Delete_Flight (Get_Input ("Identifier to Delete: "));
      Put_Line ("SUCCESS: Flight deleted.");
   end Handle_Delete_Flight;

   procedure Handle_Startup_Options is
   begin
      loop
         New_Line;
         Put_Line ("Welcome to the Flight Management System.");
         Put_Line ("Startup Options:");
         Put_Line ("  [1] Continue with existing data (Default)");
         Put_Line ("   Clear all data and start fresh");
         Put ("Choose an option: ");

         declare
            Choice : constant String := Get_Line;
         begin
            if Choice = "2" then
               Put ("ARE YOU SURE you want to delete all data? " &
                    "This cannot be undone. (y/n): ");
               if Get_Line = "y" then
                  Database_Operations.Clear_All_Data;
                  Put_Line ("SUCCESS: All data has been cleared.");
                  exit;
               else
                  Put_Line ("Operation cancelled. " &
                           "Continuing with existing data.");
                  exit;
               end if;
            elsif Choice = "1" or else Choice'Length = 0 then
               Put_Line ("Continuing with existing data...");
               exit;
            else
               Put_Line ("Invalid choice, please try again.");
            end if;
         end;
      end loop;
   end Handle_Startup_Options;

   procedure Show_Menu is
   begin
      New_Line;
      Put_Line ("---------- FLIGHT MANAGEMENT SYSTEM ----------");
      Put_Line ("Airports:    [1] Add    [2] List    Update   " &
                " Delete");
      Put_Line ("Controllers: [10] Add     List    Update   " &
                " Delete");
      Put_Line ("Flights:      Add    [A] List   [B] Update   " &
                "[C] Delete");
      Put_Line ("System:      [S] Stats  [X] Backup [E] Export   " &
                "[Q] Quit");
      Put ("Enter choice: ");
   end Show_Menu;

begin
   Database_Operations.Initialize_Database_Connection;
   Sync_Operations.Initialize_Sync_Config;

   Handle_Startup_Options;

   loop
      Show_Menu;
      Ada.Text_IO.Get (Choice);
      New_Line;

      begin
         if Choice = '1' then
            Handle_Add_Airport;
         elsif Choice = '2' then
            Handle_List_Airports;
         elsif Choice = '3' then
            Handle_Update_Airport;
         elsif Choice = '4' then
            Handle_Delete_Airport;
         elsif Choice = '5' then
            Handle_Add_Controller;
         elsif Choice = '6' then
            Handle_List_Controllers;
         elsif Choice = '7' then
            Handle_Update_Controller;
         elsif Choice = '8' then
            Handle_Delete_Controller;
         elsif Choice = '9' then
            Handle_Add_Flight;
         elsif Choice = 'A' or else Choice = 'a' then
            Handle_List_Flights;
         elsif Choice = 'B' or else Choice = 'b' then
            Handle_Update_Flight;
         elsif Choice = 'C' or else Choice = 'c' then
            Handle_Delete_Flight;
         elsif Choice = 'S' or else Choice = 's' then
            Database_Operations.Get_Database_Statistics;
         elsif Choice = 'X' or else Choice = 'x' then
            Sync_Operations.Create_Full_Backup;
         elsif Choice = 'E' or else Choice = 'e' then
            Sync_Operations.Export_All_To_JSON;
         elsif Choice = 'Q' or else Choice = 'q' then
            exit;
         else
            Put_Line ("Error: Invalid choice.");
         end if;

      exception
         when E : Database_Operations.Record_Not_Found =>
            Put_Line ("ERROR: The record to update or delete " &
                      "was not found.");
         when E : Database_Operations.Duplicate_Record | 
                  Database_Operations.Invalid_Input =>
            Put_Line ("INPUT ERROR: " & Exception_Message (E));
         when E : Database_Operations.Database_Error | 
                  Sync_Operations.Sync_Error =>
            Put_Line ("SYSTEM ERROR: " & Exception_Message (E));
         when E : Constraint_Error =>
            Put_Line ("INPUT ERROR: Invalid number format " &
                      "for a required numeric field.");
         when E : others =>
            Put_Line ("UNEXPECTED ERROR: " & Exception_Message (E));
      end;
   end loop;

   Database_Operations.Shutdown_Database_Connection;
   Put_Line ("System shut down. Goodbye.");

end Main;
