pragma Ada_2012;

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with Database_Operations;
with Sync_Operations;
with Flight_Types;

procedure Main is

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

   function Get_Input (Prompt : String) return String is
   begin
      Ada.Text_IO.Put (Prompt);
      return Ada.Strings.Fixed.Trim (Source => Ada.Text_IO.Get_Line,
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
         Max_Capacity => Get_Positive_Input ("Capacity: ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Airport added.");
   end Handle_Add_Airport;

   procedure Handle_List_Airports is
   begin
      Ada.Text_IO.Put_Line ("--- Airports List ---");
      for Item of Database_Operations.List_Airports loop
         Ada.Text_IO.Put_Line (Flight_Types.Image (Item));
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
      Ada.Text_IO.Put_Line ("SUCCESS: Airport updated.");
   end Handle_Update_Airport;

   procedure Handle_Delete_Airport is
   begin
      Database_Operations.Delete_Airport (
         By_Name => Get_Input ("Name to Delete: ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Airport deleted.");
   end Handle_Delete_Airport;

   procedure Handle_Add_Controller is
   begin
      Database_Operations.Add_Controller (
         License => Get_Input ("License: "),
         Name => Get_Input ("Name: "),
         Experience => Get_Natural_Input ("Experience (yrs): ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Controller added.");
   end Handle_Add_Controller;

   procedure Handle_List_Controllers is
   begin
      Ada.Text_IO.Put_Line ("--- Controllers List ---");
      for Item of Database_Operations.List_Controllers loop
         Ada.Text_IO.Put_Line (Flight_Types.Image (Item));
      end loop;
   end Handle_List_Controllers;

   procedure Handle_Update_Controller is
   begin
      Database_Operations.Update_Controller (
         Old_License => Get_Input ("Current License: "),
         New_Name => Get_Input ("New Name: "),
         New_Experience => Get_Natural_Input ("New Experience (yrs): ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Controller updated.");
   end Handle_Update_Controller;

   procedure Handle_Delete_Controller is
   begin
      Database_Operations.Delete_Controller (
         By_License => Get_Input ("License to Delete: ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Controller deleted.");
   end Handle_Delete_Controller;

   procedure Handle_Add_Flight is
   begin
      Database_Operations.Add_Flight (
         Identifier => Get_Input ("Identifier: "),
         Origin_Airport_Name => Get_Input ("Origin Airport: "),
         Dest_Airport_Name => Get_Input ("Destination Airport: ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Flight added.");
   end Handle_Add_Flight;

   procedure Handle_List_Flights is
   begin
      Ada.Text_IO.Put_Line ("--- Flights List ---");
      for Item of Database_Operations.List_Flights loop
         Ada.Text_IO.Put_Line (Flight_Types.Image (Item));
      end loop;
   end Handle_List_Flights;

   procedure Handle_Update_Flight is
   begin
      Database_Operations.Update_Flight (
         Old_Identifier => Get_Input ("Current Identifier: "),
         New_Origin_Name => Get_Input ("New Origin Airport: "),
         New_Dest_Name => Get_Input ("New Destination Airport: ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Flight updated.");
   end Handle_Update_Flight;

   procedure Handle_Delete_Flight is
   begin
      Database_Operations.Delete_Flight (
         By_Identifier => Get_Input ("Identifier to Delete: ")
      );
      Ada.Text_IO.Put_Line ("SUCCESS: Flight deleted.");
   end Handle_Delete_Flight;

   procedure Handle_Startup_Options is
   begin
      loop
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Welcome to the Flight Management System.");
         Ada.Text_IO.Put_Line ("Startup Options:");
         Ada.Text_IO.Put_Line ("  [1] Continue with existing data (Default)");
         Ada.Text_IO.Put_Line ("   Clear all data and start fresh");
         Ada.Text_IO.Put ("Choose an option: ");

         declare
            Choice : constant String := Ada.Text_IO.Get_Line;
         begin
            if Choice = "2" then
               Ada.Text_IO.Put ("ARE YOU SURE you want to delete all data? " &
                                "This cannot be undone. (y/n): ");
               if Ada.Text_IO.Get_Line = "y" then
                  Database_Operations.Clear_All_Data;
                  Ada.Text_IO.Put_Line ("SUCCESS: All data has been cleared.");
                  exit;
               else
                  Ada.Text_IO.Put_Line ("Operation cancelled.");
                  exit;
               end if;
            elsif Choice = "1" or else Choice'Length = 0 then
               Ada.Text_IO.Put_Line ("Continuing with existing data...");
               exit;
            else
               Ada.Text_IO.Put_Line ("Invalid choice, please try again.");
            end if;
         end;
      end loop;
   end Handle_Startup_Options;

   procedure Show_Menu is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("---------- FLIGHT MANAGEMENT SYSTEM ----------");
      Ada.Text_IO.Put_Line ("Airports:    [1] Add     List   " &
                            " Update    Delete");
      Ada.Text_IO.Put_Line ("Controllers: [9] Add     List   " &
                            " Update    Delete");
      Ada.Text_IO.Put_Line ("Flights:      Add    [A] List   " &
                            "[B] Update   [C] Delete");
      Ada.Text_IO.Put_Line ("System:      [S] Stats  [X] Backup " &
                            "[E] Export   [Q] Quit");
      Ada.Text_IO.Put ("Enter choice: ");
   end Show_Menu;

begin
   Database_Operations.Initialize_Database_Connection;
   Sync_Operations.Initialize_Sync_Config;

   Handle_Startup_Options;

   loop
      Show_Menu;
      Ada.Text_IO.Get (Choice);
      Ada.Text_IO.New_Line;

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
            Ada.Text_IO.Put_Line ("Error: Invalid choice.");
         end if;

      exception
         when Database_Operations.Record_Not_Found =>
            Ada.Text_IO.Put_Line ("ERROR: The record to update or delete " &
                                  "was not found.");
         when Database_Operations.Duplicate_Record |
              Database_Operations.Invalid_Input =>
            Ada.Text_IO.Put_Line ("INPUT ERROR: Invalid data provided.");
         when Database_Operations.Database_Error |
              Sync_Operations.Sync_Error =>
            Ada.Text_IO.Put_Line ("SYSTEM ERROR: Database or sync operation failed.");
         when Constraint_Error =>
            Ada.Text_IO.Put_Line ("INPUT ERROR: Invalid number format " &
                                  "for a required numeric field.");
         when others =>
            Ada.Text_IO.Put_Line ("UNEXPECTED ERROR: An unknown error occurred.");
      end;
   end loop;

   Database_Operations.Shutdown_Database_Connection;
   Ada.Text_IO.Put_Line ("System shut down. Goodbye.");

end Main;
