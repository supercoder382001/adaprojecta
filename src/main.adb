pragma Ada_2012;
with Ada.Text_IO;
with Database_Operations; use Database_Operations;
with Flight_Types; use Flight_Types;
with Sync_Operations; use Sync_Operations;

procedure Main is

   procedure Show_Menu;
   procedure Handle_Add_Airport;
   procedure Handle_List_Airports;
   procedure Handle_Add_Controller;
   procedure Handle_List_Controllers;
   procedure Handle_Add_Flight;
   procedure Handle_List_Flights;
   procedure Handle_Delete_Airport;
   procedure Handle_Delete_Controller;
   procedure Handle_Delete_Flight;

   Choice_Input  : String (1 .. 10);
   Choice_Length : Natural;
   Running       : Boolean := True;

   procedure Show_Menu is
   begin
      Ada.Text_IO.Put_Line ("=== Flight Management System ===");
      Ada.Text_IO.Put_Line ("[1] Add Airport      [1] List Airports");
      Ada.Text_IO.Put_Line (" Update Airport    Delete Airport");
      Ada.Text_IO.Put_Line ("[2] Add Controller    List Controllers");
      Ada.Text_IO.Put_Line (" Update Controller  Delete Controller");
      Ada.Text_IO.Put_Line (" Add Flight       [A] List Flights");
      Ada.Text_IO.Put_Line ("[B] Update Flight    [D] Delete Flight");
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
      Save_All_Data;  -- Auto-save after adding
   exception
      when Duplicate_Record =>
         Ada.Text_IO.Put_Line ("ERROR: Airport already exists!");
      when others =>
         Ada.Text_IO.Put_Line ("ERROR: Failed to add airport!");
   end Handle_Add_Airport;

   procedure Handle_List_Airports is
      Airports : constant Airport_Vectors.Vector := List_Airports;
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
                                           "
