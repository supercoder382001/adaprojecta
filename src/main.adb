pragma Ada_2012;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Database_Operations; use Database_Operations;
with Flight_Types; use Flight_Types;
with Sync_Operations; use Sync_Operations;

procedure Main is

   procedure Show_Menu is
   begin
      Ada.Text_IO.Put_Line ("=== Flight Management System ===");
      Ada.Text_IO.Put_Line ("[1] Add Airport      [2] List Airports");
      Ada.Text_IO.Put_Line (" Update Airport    Delete Airport");
      Ada.Text_IO.Put_Line ("[1] Add Controller    List Controllers");
      Ada.Text_IO.Put_Line (" Update Controller  Delete Controller");
      Ada.Text_IO.Put_Line (" Add Flight        List Flights");
      Ada.Text_IO.Put_Line (" Update Flight    Delete Flight");
      Ada.Text_IO.Put_Line ("[C] Clear All Data   [S] Statistics");
      Ada.Text_IO.Put_Line ("[E] Export to JSON   [X] Backup Data");
      Ada.Text_IO.Put_Line ("[Q] Quit");
      Ada.Text_IO.New_Line;
   end Show_Menu;

   procedure Handle_Add_Airport is
      Name_Input      : String (1 .. 100);
      Name_Length     : Natural;
      Location_Input  : String (1 .. 100);
      Location_Length : Natural;
      Capacity_Input  : String (1 .. 10);
      Capacity_Length : Natural;
      Capacity_Value  : Positive;
      Valid_Capacity  : Boolean := False;
   begin
      Ada.Text_IO.Put ("Enter Airport Name: ");
      Ada.Text_IO.Get_Line (Name_Input, Name_Length);

      Ada.Text_IO.Put ("Enter Airport Location: ");
      Ada.Text_IO.Get_Line (Location_Input, Location_Length);

      while not Valid_Capacity loop
         Ada.Text_IO.Put ("Enter Capacity (positive integer): ");
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
                  Valid_Capacity := True;
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
      Ada.Text_IO.Put_Line ("Airport added successfully!");
   exception
      when Duplicate_Record =>
         Ada.Text_IO.Put_Line ("ERROR: Airport already exists!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to add airport!");
   end Handle_Add_Airport;

   procedure Handle_List_Airports is
      Airports : Airport_Vectors.Vector := List_Airports;
   begin
      Ada.Text_IO.Put_Line ("=== Airports ===");
      if Airports.Is_Empty then
         Ada.Text_IO.Put_Line ("No airports found.");
      else
         for Airport of Airports loop
            Ada.Text_IO.Put_Line (Image (Airport));
         end loop;
      end if;
   end Handle_List_Airports;

   procedure Handle_Add_Controller is
      License_Input     : String (1 .. 50);
      License_Length    : Natural;
      Name_Input        : String (1 .. 100);
      Name_Length       : Natural;
      Experience_Input  : String (1 .. 10);
      Experience_Length : Natural;
      Experience_Value  : Natural;
      Valid_Experience  : Boolean := False;
   begin
      Ada.Text_IO.Put ("Enter License Number: ");
      Ada.Text_IO.Get_Line (License_Input, License_Length);

      Ada.Text_IO.Put ("Enter Controller Name: ");
      Ada.Text_IO.Get_Line (Name_Input, Name_Length);

      while not Valid_Experience loop
         Ada.Text_IO.Put ("Enter Experience Years (0-50): ");
         Ada.Text_IO.Get_Line (Experience_Input, Experience_Length);

         if Experience_Length > 0 then
            declare
               Tmp_Str  : String := Experience_Input (1 .. Experience_Length);
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
                     Valid_Experience := True;
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
      Ada.Text_IO.Put_Line ("Controller added successfully!");
   exception
      when Duplicate_Record =>
         Ada.Text_IO.Put_Line ("ERROR: Controller license already exists!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to add controller!");
   end Handle_Add_Controller;

   procedure Handle_List_Controllers is
      Controllers : Controller_Vectors.Vector := List_Controllers;
   begin
      Ada.Text_IO.Put_Line ("=== Controllers ===");
      if Controllers.Is_Empty then
         Ada.Text_IO.Put_Line ("No controllers found.");
      else
         for Controller of Controllers loop
            Ada.Text_IO.Put_Line (Image (Controller));
         end loop;
      end if;
   end Handle_List_Controllers;

   procedure Handle_Add_Flight is
      Identifier_Input  : String (1 .. 50);
      Identifier_Length : Natural;
      Origin_Input      : String (1 .. 100);
      Origin_Length     : Natural;
      Dest_Input        : String (1 .. 100);
      Dest_Length       : Natural;
   begin
      Ada.Text_IO.Put ("Enter Flight Identifier: ");
      Ada.Text_IO.Get_Line (Identifier_Input, Identifier_Length);

      Ada.Text_IO.Put ("Enter Origin Airport: ");
      Ada.Text_IO.Get_Line (Origin_Input, Origin_Length);

      Ada.Text_IO.Put ("Enter Destination Airport: ");
      Ada.Text_IO.Get_Line (Dest_Input, Dest_Length);

      Add_Flight (Identifier_Input (1 .. Identifier_Length),
                  Origin_Input (1 .. Origin_Length),
                  Dest_Input (1 .. Dest_Length));
      Ada.Text_IO.Put_Line ("Flight added successfully!");
   exception
      when Duplicate_Record =>
         Ada.Text_IO.Put_Line ("ERROR: Flight identifier already exists!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to add flight!");
   end Handle_Add_Flight;

   procedure Handle_List_Flights is
      Flights : Flight_Vectors.Vector := List_Flights;
   begin
      Ada.Text_IO.Put_Line ("=== Flights ===");
      if Flights.Is_Empty then
         Ada.Text_IO.Put_Line ("No flights found.");
      else
         for Flight of Flights loop
            Ada.Text_IO.Put_Line (Image (Flight));
         end loop;
      end if;
   end Handle_List_Flights;

   procedure Handle_Delete_Airport is
      Name_Input  : String (1 .. 100);
      Name_Length : Natural;
   begin
      Ada.Text_IO.Put ("Enter Airport Name to Delete: ");
      Ada.Text_IO.Get_Line (Name_Input, Name_Length);

      Delete_Airport (Name_Input (1 .. Name_Length));
      Ada.Text_IO.Put_Line ("Airport deleted successfully!");
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Airport not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to delete airport!");
   end Handle_Delete_Airport;

   procedure Handle_Delete_Controller is
      License_Input  : String (1 .. 50);
      License_Length : Natural;
   begin
      Ada.Text_IO.Put ("Enter License Number to Delete: ");
      Ada.Text_IO.Get_Line (License_Input, License_Length);

      Delete_Controller (License_Input (1 .. License_Length));
      Ada.Text_IO.Put_Line ("Controller deleted successfully!");
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Controller not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to delete controller!");
   end Handle_Delete_Controller;

   procedure Handle_Delete_Flight is
      Identifier_Input  : String (1 .. 50);
      Identifier_Length : Natural;
   begin
      Ada.Text_IO.Put ("Enter Flight Identifier to Delete: ");
      Ada.Text_IO.Get_Line (Identifier_Input, Identifier_Length);

      Delete_Flight (Identifier_Input (1 .. Identifier_Length));
      Ada.Text_IO.Put_Line ("Flight deleted successfully!");
   exception
      when Record_Not_Found =>
         Ada.Text_IO.Put_Line ("ERROR: Flight not found!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to delete flight!");
   end Handle_Delete_Flight;

   Choice_Input  : String (1 .. 10);
   Choice_Length : Natural;
   Running       : Boolean := True;

begin
   Initialize_Database_Connection;
   Initialize_Sync_Config;

   Ada.Text_IO.Put_Line ("Welcome to Flight Management System!");
   Ada.Text_IO.New_Line;

   while Running loop
      Show_Menu;
      Ada.Text_IO.Put ("Enter your choice: ");
      Ada.Text_IO.Get_Line (Choice_Input, Choice_Length);

      if Choice_Length > 0 then
         if Choice_Input (1 .. Choice_Length) = "1" then
            Handle_Add_Airport;

         elsif Choice_Input (1 .. Choice_Length) = "2" then
            Handle_List_Airports;

         elsif Choice_Input (1 .. Choice_Length) = "3" then
            declare
               Old_Name_Input       : String (1 .. 100);
               Old_Name_Length      : Natural;
               New_Name_Input       : String (1 .. 100);
               New_Name_Length      : Natural;
               New_Location_Input   : String (1 .. 100);
               New_Location_Length  : Natural;
               New_Capacity_Input   : String (1 .. 10);
               New_Capacity_Length  : Natural;
               New_Capacity_Value   : Positive;
               Valid_Capacity       : Boolean := False;
            begin
               Ada.Text_IO.Put ("Enter Current Airport Name: ");
               Ada.Text_IO.Get_Line (Old_Name_Input, Old_Name_Length);

               Ada.Text_IO.Put ("Enter New Airport Name: ");
               Ada.Text_IO.Get_Line (New_Name_Input, New_Name_Length);

               Ada.Text_IO.Put ("Enter New Location: ");
               Ada.Text_IO.Get_Line (New_Location_Input, New_Location_Length);

               while not Valid_Capacity loop
                  Ada.Text_IO.Put ("Enter New Capacity: ");
                  Ada.Text_IO.Get_Line (New_Capacity_Input,
                                        New_Capacity_Length);

                  if New_Capacity_Length > 0 then
                     declare
                        Tmp_Str : String := New_Capacity_Input 
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
                           Valid_Capacity := True;
                        else
                           Ada.Text_IO.Put_Line ("ERROR: Please enter only " &
                                                 "positive numbers");
                        end if;
                     exception
                        when others =>
                           Ada.Text_IO.Put_Line ("ERROR: Invalid number " &
                                                 "format");
                     end;
                  else
                     Ada.Text_IO.Put_Line ("ERROR: Capacity cannot be empty");
                  end if;
               end loop;

               Update_Airport (Old_Name_Input (1 .. Old_Name_Length),
                               New_Name_Input (1 .. New_Name_Length),
                               New_Location_Input (1 .. New_Location_Length),
                               New_Capacity_Value);
               Ada.Text_IO.Put_Line ("Airport updated successfully!");
            exception
               when Record_Not_Found =>
                  Ada.Text_IO.Put_Line ("ERROR: Airport not found!");
               when others =>
                  Ada.Text_IO.Put_Line ("ERROR: Failed to update airport!");
            end;

         elsif Choice_Input (1 .. Choice_Length) = "4" then
            Handle_Delete_Airport;

         elsif Choice_Input (1 .. Choice_Length) = "5" then
            Handle_Add_Controller;

         elsif Choice_Input (1 .. Choice_Length) = "6" then
            Handle_List_Controllers;

         elsif Choice_Input (1 .. Choice_Length) = "7" then
            declare
               Old_License_Input     : String (1 .. 50);
               Old_License_Length    : Natural;
               New_Name_Input        : String (1 .. 100);
               New_Name_Length       : Natural;
               New_Experience_Input  : String (1 .. 10);
               New_Experience_Length : Natural;
               New_Experience_Value  : Natural;
               Valid_Experience      : Boolean := False;
            begin
               Ada.Text_IO.Put ("Enter Current License Number: ");
               Ada.Text_IO.Get_Line (Old_License_Input, Old_License_Length);

               Ada.Text_IO.Put ("Enter New Controller Name: ");
               Ada.Text_IO.Get_Line (New_Name_Input, New_Name_Length);

               while not Valid_Experience loop
                  Ada.Text_IO.Put ("Enter New Experience Years: ");
                  Ada.Text_IO.Get_Line (New_Experience_Input,
                                        New_Experience_Length);

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
                           Valid_Experience := True;
                        else
                           Ada.Text_IO.Put_Line ("ERROR: Please enter only " &
                                                 "numbers");
                        end if;
                     exception
                        when others =>
                           Ada.Text_IO.Put_Line ("ERROR: Invalid number " &
                                                 "format");
                     end;
                  else
                     Ada.Text_IO.Put_Line ("ERROR: Experience cannot be " &
                                           "empty");
                  end if;
               end loop;

               Update_Controller (Old_License_Input (1 .. Old_License_Length),
                                  New_Name_Input (1 .. New_Name_Length),
                                  New_Experience_Value);
               Ada.Text_IO.Put_Line ("Controller updated successfully!");
            exception
               when Record_Not_Found =>
                  Ada.Text_IO.Put_Line ("ERROR: Controller not found!");
               when others =>
                  Ada.Text_IO.Put_Line ("ERROR: Failed to update " &
                                        "controller!");
            end;

         elsif Choice_Input (1 .. Choice_Length) = "8" then
            Handle_Delete_Controller;

         elsif Choice_Input (1 .. Choice_Length) = "9" then
            Handle_Add_Flight;

         elsif Choice_Input (1 .. Choice_Length) = "10" then
            Handle_List_Flights;

         elsif Choice_Input (1 .. Choice_Length) = "11" then
            declare
               Old_Identifier_Input  : String (1 .. 50);
               Old_Identifier_Length : Natural;
               New_Origin_Input      : String (1 .. 100);
               New_Origin_Length     : Natural;
               New_Dest_Input        : String (1 .. 100);
               New_Dest_Length       : Natural;
            begin
               Ada.Text_IO.Put ("Enter Current Flight Identifier: ");
               Ada.Text_IO.Get_Line (Old_Identifier_Input,
                                     Old_Identifier_Length);

               Ada.Text_IO.Put ("Enter New Origin Airport: ");
               Ada.Text_IO.Get_Line (New_Origin_Input, New_Origin_Length);

               Ada.Text_IO.Put ("Enter New Destination Airport: ");
               Ada.Text_IO.Get_Line (New_Dest_Input, New_Dest_Length);

               Update_Flight (Old_Identifier_Input (1 .. Old_Identifier_Length),
                              New_Origin_Input (1 .. New_Origin_Length),
                              New_Dest_Input (1 .. New_Dest_Length));
               Ada.Text_IO.Put_Line ("Flight updated successfully!");
            exception
               when Record_Not_Found =>
                  Ada.Text_IO.Put_Line ("ERROR: Flight not found!");
               when others =>
                  Ada.Text_IO.Put_Line ("ERROR: Failed to update flight!");
            end;

         elsif Choice_Input (1 .. Choice_Length) = "12" then
            Handle_Delete_Flight;

         elsif Choice_Input (1) = 'C' or Choice_Input (1) = 'c' then
            Clear_All_Data;
            Ada.Text_IO.Put_Line ("All data cleared successfully!");

         elsif Choice_Input (1) = 'S' or Choice_Input (1) = 's' then
            Get_Database_Statistics;

         elsif Choice_Input (1) = 'E' or Choice_Input (1) = 'e' then
            Export_All_To_JSON;

         elsif Choice_Input (1) = 'X' or Choice_Input (1) = 'x' then
            Create_Full_Backup;

         elsif Choice_Input (1) = 'Q' or Choice_Input (1) = 'q' then
            Ada.Text_IO.Put_Line ("Shutting down...");
            Running := False;

         else
            Ada.Text_IO.Put_Line ("Invalid choice. Please try again.");
         end if;
      else
         Ada.Text_IO.Put_Line ("Please enter a valid choice.");
      end if;

      if Running then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Press Enter to continue...");
         Ada.Text_IO.Skip_Line;
      end if;
   end loop;

   Shutdown_Database_Connection;
   Ada.Text_IO.Put_Line ("Goodbye!");

exception
   when others =>
      Ada.Text_IO.Put_Line ("Fatal error occurred. Shutting down.");
      Shutdown_Database_Connection;
end Main;
