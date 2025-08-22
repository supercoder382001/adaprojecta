pragma Ada_2012;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Flight_Types; use Flight_Types;

package body Database_Operations is

   Airports    : Airport_Vectors.Vector;
   Controllers : Controller_Vectors.Vector;
   Flights     : Flight_Vectors.Vector;

   DB_Connected : Boolean := False;

   procedure Initialize_Database_Connection is
   begin
      if not Ada.Directories.Exists ("data/") then
         Ada.Directories.Create_Path ("data/");
      end if;

      begin
         if Ada.Directories.Exists ("/usr/bin/psql") or else
            Ada.Directories.Exists ("/usr/local/bin/psql")
         then
            DB_Connected := True;
            Ada.Text_IO.Put_Line ("PostgreSQL detected - " &
                                  "flight_user@flight_management ready.");
         else
            DB_Connected := False;
            Ada.Text_IO.Put_Line ("PostgreSQL not detected. " &
                                  "Using file storage only.");
         end if;
      exception
         when others =>
            DB_Connected := False;
            Ada.Text_IO.Put_Line ("Database connection check failed. " &
                                  "Using file storage only.");
      end;

      Ada.Text_IO.Put_Line ("Database system initialized (File " &
                            (if DB_Connected then "+ PostgreSQL)"
                             else "only)"));
   end Initialize_Database_Connection;

   procedure Shutdown_Database_Connection is
   begin
      if DB_Connected then
         Ada.Text_IO.Put_Line ("PostgreSQL session ended.");
      end if;
      Ada.Text_IO.Put_Line ("Database connections closed.");
   end Shutdown_Database_Connection;

   procedure Clear_All_Data is
   begin
      Airports.Clear;
      Controllers.Clear;
      Flights.Clear;
      Save_All_Data;
      Ada.Text_IO.Put_Line ("All data cleared from file and database.");
   end Clear_All_Data;

   procedure Save_To_File is
      Data_File : Ada.Text_IO.File_Type;
      Data_Path : constant String := "data/flight_data.txt";
   begin
      Ada.Text_IO.Create (Data_File, Ada.Text_IO.Out_File, Data_Path);

      Ada.Text_IO.Put_Line (Data_File, "AIRPORTS:" & Airports.Length'Image);
      for A of Airports loop
         Ada.Text_IO.Put_Line (Data_File,
            To_String (A.Name) & "|" &
            To_String (A.Location) & "|" &
            Ada.Strings.Fixed.Trim (A.Max_Capacity'Image,
                                    Ada.Strings.Left));
      end loop;

      Ada.Text_IO.Put_Line (Data_File, "CONTROLLERS:" &
                                       Controllers.Length'Image);
      for C of Controllers loop
         Ada.Text_IO.Put_Line (Data_File,
            To_String (C.License_Number) & "|" &
            To_String (C.Name) & "|" &
            Ada.Strings.Fixed.Trim (C.Experience_Years'Image,
                                    Ada.Strings.Left));
      end loop;

      Ada.Text_IO.Put_Line (Data_File, "FLIGHTS:" & Flights.Length'Image);
      for F of Flights loop
         Ada.Text_IO.Put_Line (Data_File, To_String (F.Identifier) & "|" &
                                          To_String (F.Origin_Name) & "|" &
                                          To_String (F.Destination_Name));
      end loop;

      Ada.Text_IO.Close (Data_File);
      Ada.Text_IO.Put_Line ("Data saved to file: " & Data_Path);
   exception
      when others =>
         if Ada.Text_IO.Is_Open (Data_File) then
            Ada.Text_IO.Close (Data_File);
         end if;
         Ada.Text_IO.Put_Line ("Warning: Failed to save to file");
   end Save_To_File;

   procedure Save_To_Database is
      SQL_File : Ada.Text_IO.File_Type;
      SQL_Path : constant String := "data/sync_to_db.sql";
   begin
      if not DB_Connected then
         return;
      end if;

      Ada.Text_IO.Create (SQL_File, Ada.Text_IO.Out_File, SQL_Path);

      Ada.Text_IO.Put_Line (SQL_File, "-- Sync Ada data to PostgreSQL");
      Ada.Text_IO.Put_Line (SQL_File, "-- Execute: " &
                                      "psql -U flight_user " &
                                      "-d flight_management -f " & SQL_Path);
      Ada.Text_IO.Put_Line (SQL_File, "");

      Ada.Text_IO.Put_Line (SQL_File, "-- Clear existing data");
      Ada.Text_IO.Put_Line (SQL_File, "DELETE FROM controllers WHERE " &
                                      "assigned_flight_id IS NOT NULL;");
      Ada.Text_IO.Put_Line (SQL_File, "DELETE FROM flights;");
      Ada.Text_IO.Put_Line (SQL_File, "DELETE FROM controllers;");
      Ada.Text_IO.Put_Line (SQL_File, "DELETE FROM airports;");
      Ada.Text_IO.Put_Line (SQL_File, "");

      Ada.Text_IO.Put_Line (SQL_File, "-- Insert airports using your function");
      for A of Airports loop
         Ada.Text_IO.Put_Line (SQL_File, "SELECT add_airport('" &
                                        To_String (A.Name) & "', '" &
                                        To_String (A.Location) & "', " &
                                        Ada.Strings.Fixed.Trim (
                                           A.Max_Capacity'Image,
                                           Ada.Strings.Left) & ");");
      end loop;

      Ada.Text_IO.Put_Line (SQL_File, "");
      Ada.Text_IO.Put_Line (SQL_File, "-- Insert controllers using " &
                                      "your function");
      for C of Controllers loop
         Ada.Text_IO.Put_Line (SQL_File, "SELECT add_controller('" &
                                        To_String (C.License_Number) & "', '" &
                                        To_String (C.Name) & "', " &
                                        Ada.Strings.Fixed.Trim (
                                           C.Experience_Years'Image,
                                           Ada.Strings.Left) & ");");
      end loop;

      Ada.Text_IO.Put_Line (SQL_File, "");
      Ada.Text_IO.Put_Line (SQL_File, "-- Insert flights using your function");
      for F of Flights loop
         Ada.Text_IO.Put_Line (SQL_File, "SELECT add_flight('" &
                                        To_String (F.Identifier) & "', '" &
                                        To_String (F.Origin_Name) & "', '" &
                                        To_String (F.Destination_Name) & "');");
      end loop;

      Ada.Text_IO.Close (SQL_File);
      Ada.Text_IO.Put_Line ("Sync file generated: " & SQL_Path);
      Ada.Text_IO.Put_Line ("Run: psql -U flight_user " &
                            "-d flight_management -f " & SQL_Path);
   exception
      when others =>
         if Ada.Text_IO.Is_Open (SQL_File) then
            Ada.Text_IO.Close (SQL_File);
         end if;
         Ada.Text_IO.Put_Line ("Warning: Failed to generate sync file");
   end Save_To_Database;

   procedure Load_From_File is
      Data_File : Ada.Text_IO.File_Type;
      Data_Path : constant String := "data/flight_data.txt";
      Line : String (1 .. 500);
      Line_Length : Natural;
   begin
      if not Ada.Directories.Exists (Data_Path) then
         Ada.Text_IO.Put_Line ("No saved file data found.");
         return;
      end if;

      Airports.Clear;
      Controllers.Clear;
      Flights.Clear;

      Ada.Text_IO.Open (Data_File, Ada.Text_IO.In_File, Data_Path);

      while not Ada.Text_IO.End_Of_File (Data_File) loop
         Ada.Text_IO.Get_Line (Data_File, Line, Line_Length);

         if Line_Length > 9 and then Line (1 .. 9) = "AIRPORTS:" then
            declare
               Count_Str : constant String := Ada.Strings.Fixed.Trim (
                  Line (10 .. Line_Length), Ada.Strings.Both);
               Count : constant Natural := Natural'Value (Count_Str);
            begin
               for I in 1 .. Count loop
                  Ada.Text_IO.Get_Line (Data_File, Line, Line_Length);
                  declare
                     Line_Str : constant String := Line (1 .. Line_Length);
                     Pipe1, Pipe2 : Natural := 0;
                     New_Airport : Airport_Record;
                  begin
                     for J in Line_Str'Range loop
                        if Line_Str (J) = '|' then
                           if Pipe1 = 0 then
                              Pipe1 := J;
                           else
                              Pipe2 := J;
                              exit;
                           end if;
                        end if;
                     end loop;

                     New_Airport.Name := To_Unbounded_String (
                        Line_Str (1 .. Pipe1 - 1));
                     New_Airport.Location := To_Unbounded_String (
                        Line_Str (Pipe1 + 1 .. Pipe2 - 1));
                     New_Airport.Max_Capacity := Positive'Value (
                        Line_Str (Pipe2 + 1 .. Line_Length));
                     Airports.Append (New_Airport);
                  end;
               end loop;
            end;

         elsif Line_Length > 12 and then Line (1 .. 12) = "CONTROLLERS:" then
            declare
               Count_Str : constant String := Ada.Strings.Fixed.Trim (
                  Line (13 .. Line_Length), Ada.Strings.Both);
               Count : constant Natural := Natural'Value (Count_Str);
            begin
               for I in 1 .. Count loop
                  Ada.Text_IO.Get_Line (Data_File, Line, Line_Length);
                  declare
                     Line_Str : constant String := Line (1 .. Line_Length);
                     Pipe1, Pipe2 : Natural := 0;
                     New_Controller : Controller_Record;
                  begin
                     for J in Line_Str'Range loop
                        if Line_Str (J) = '|' then
                           if Pipe1 = 0 then
                              Pipe1 := J;
                           else
                              Pipe2 := J;
                              exit;
                           end if;
                        end if;
                     end loop;

                     New_Controller.License_Number := To_Unbounded_String (
                        Line_Str (1 .. Pipe1 - 1));
                     New_Controller.Name := To_Unbounded_String (
                        Line_Str (Pipe1 + 1 .. Pipe2 - 1));
                     New_Controller.Experience_Years := Natural'Value (
                        Line_Str (Pipe2 + 1 .. Line_Length));
                     Controllers.Append (New_Controller);
                  end;
               end loop;
            end;

         elsif Line_Length > 8 and then Line (1 .. 8) = "FLIGHTS:" then
            declare
               Count_Str : constant String := Ada.Strings.Fixed.Trim (
                  Line (9 .. Line_Length), Ada.Strings.Both);
               Count : constant Natural := Natural'Value (Count_Str);
            begin
               for I in 1 .. Count loop
                  Ada.Text_IO.Get_Line (Data_File, Line, Line_Length);
                  declare
                     Line_Str : constant String := Line (1 .. Line_Length);
                     Pipe1, Pipe2 : Natural := 0;
                     New_Flight : Flight_Record;
                  begin
                     for J in Line_Str'Range loop
                        if Line_Str (J) = '|' then
                           if Pipe1 = 0 then
                              Pipe1 := J;
                           else
                              Pipe2 := J;
                              exit;
                           end if;
                        end if;
                     end loop;

                     New_Flight.Identifier := To_Unbounded_String (
                        Line_Str (1 .. Pipe1 - 1));
                     New_Flight.Origin_Name := To_Unbounded_String (
                        Line_Str (Pipe1 + 1 .. Pipe2 - 1));
                     New_Flight.Destination_Name := To_Unbounded_String (
                        Line_Str (Pipe2 + 1 .. Line_Length));
                     Flights.Append (New_Flight);
                  end;
               end loop;
            end;
         end if;
      end loop;

      Ada.Text_IO.Close (Data_File);
      Ada.Text_IO.Put_Line ("Data loaded from file.");
   exception
      when others =>
         if Ada.Text_IO.Is_Open (Data_File) then
            Ada.Text_IO.Close (Data_File);
         end if;
         Ada.Text_IO.Put_Line ("Warning: Failed to load data from file.");
   end Load_From_File;

   procedure Load_From_Database is
      Query_File : Ada.Text_IO.File_Type;
      Query_Path : constant String := "data/load_from_db.sql";
   begin
      if DB_Connected then
         Ada.Text_IO.Create (Query_File, Ada.Text_IO.Out_File, Query_Path);
         Ada.Text_IO.Put_Line (Query_File, "-- Load data from PostgreSQL");
         Ada.Text_IO.Put_Line (Query_File, "-- Use your schema functions:");
         Ada.Text_IO.Put_Line (Query_File, "SELECT * FROM list_airports();");
         Ada.Text_IO.Put_Line (Query_File,
                               "SELECT * FROM list_controllers();");
         Ada.Text_IO.Put_Line (Query_File, "SELECT * FROM list_flights();");
         Ada.Text_IO.Close (Query_File);

         Ada.Text_IO.Put_Line ("Query file generated: " & Query_Path);
         Ada.Text_IO.Put_Line ("Run: psql -U flight_user " &
                               "-d flight_management -f " & Query_Path);
      else
         Ada.Text_IO.Put_Line ("No database connection available.");
      end if;
   end Load_From_Database;

   procedure Clear_Database is
      Clear_File : Ada.Text_IO.File_Type;
      Clear_Path : constant String := "data/clear_db.sql";
   begin
      if DB_Connected then
         Ada.Text_IO.Create (Clear_File, Ada.Text_IO.Out_File, Clear_Path);
         Ada.Text_IO.Put_Line (Clear_File,
                               "-- Clear all data from PostgreSQL");
         Ada.Text_IO.Put_Line (Clear_File, "DELETE FROM controllers WHERE " &
                                          "assigned_flight_id IS NOT NULL;");
         Ada.Text_IO.Put_Line (Clear_File, "DELETE FROM flights;");
         Ada.Text_IO.Put_Line (Clear_File, "DELETE FROM controllers;");
         Ada.Text_IO.Put_Line (Clear_File, "DELETE FROM airports;");
         Ada.Text_IO.Close (Clear_File);

         Ada.Text_IO.Put_Line ("Clear script generated: " & Clear_Path);
         Ada.Text_IO.Put_Line ("Run: psql -U flight_user " &
                               "-d flight_management -f " & Clear_Path);
      end if;
   end Clear_Database;

   procedure Save_All_Data is
   begin
      Save_To_File;
      if DB_Connected then
         Save_To_Database;
      end if;
   end Save_All_Data;

   procedure Load_All_Data is
   begin
      Load_From_File;
   end Load_All_Data;

   procedure Add_Airport (Name : String; Location : String;
                         Max_Capacity : Positive) is
      New_Airport : Airport_Record;
   begin
      for A of Airports loop
         if To_String (A.Name) = Name then
            raise Duplicate_Record with "Airport already exists: " & Name;
         end if;
      end loop;

      New_Airport.Name := To_Unbounded_String (Name);
      New_Airport.Location := To_Unbounded_String (Location);
      New_Airport.Max_Capacity := Max_Capacity;
      Airports.Append (New_Airport);
   end Add_Airport;

   function List_Airports return Flight_Types.Airport_Vectors.Vector is
   begin
      return Airports;
   end List_Airports;

   procedure Update_Airport (Old_Name, New_Name, New_Location : String;
                            New_Capacity : Positive) is
   begin
      for I in Airports.First_Index .. Airports.Last_Index loop
         if To_String (Airports.Element (I).Name) = Old_Name then
            declare
               Updated : Airport_Record := Airports.Element (I);
            begin
               Updated.Name := To_Unbounded_String (New_Name);
               Updated.Location := To_Unbounded_String (New_Location);
               Updated.Max_Capacity := New_Capacity;
               Airports.Replace_Element (I, Updated);
               return;
            end;
         end if;
      end loop;
      raise Record_Not_Found with "Airport not found: " & Old_Name;
   end Update_Airport;

   procedure Delete_Airport (By_Name : String) is
   begin
      for I in Airports.First_Index .. Airports.Last_Index loop
         if To_String (Airports.Element (I).Name) = By_Name then
            Airports.Delete (I);
            return;
         end if;
      end loop;
      raise Record_Not_Found with "Airport not found: " & By_Name;
   end Delete_Airport;

   procedure Add_Controller (License, Name : String; Experience : Natural) is
      New_Controller : Controller_Record;
   begin
      for C of Controllers loop
         if To_String (C.License_Number) = License then
            raise Duplicate_Record with "Controller already exists: " &
                                        License;
         end if;
      end loop;

      New_Controller.License_Number := To_Unbounded_String (License);
      New_Controller.Name := To_Unbounded_String (Name);
      New_Controller.Experience_Years := Experience;
      Controllers.Append (New_Controller);
   end Add_Controller;

   function List_Controllers return Flight_Types.Controller_Vectors.Vector is
   begin
      return Controllers;
   end List_Controllers;

   procedure Update_Controller (Old_License, New_Name : String;
                               New_Experience : Natural) is
      Current_License : Unbounded_String;
   begin
      for I in Controllers.First_Index .. Controllers.Last_Index loop
         Current_License := Controllers.Element (I).License_Number;
         if To_String (Current_License) = Old_License then
            declare
               Updated : Controller_Record := Controllers.Element (I);
            begin
               Updated.Name := To_Unbounded_String (New_Name);
               Updated.Experience_Years := New_Experience;
               Controllers.Replace_Element (I, Updated);
               return;
            end;
         end if;
      end loop;
      raise Record_Not_Found with "Controller not found: " & Old_License;
   end Update_Controller;

   procedure Delete_Controller (By_License : String) is
      Current_License : Unbounded_String;
   begin
      for I in Controllers.First_Index .. Controllers.Last_Index loop
         Current_License := Controllers.Element (I).License_Number;
         if To_String (Current_License) = By_License then
            Controllers.Delete (I);
            return;
         end if;
      end loop;
      raise Record_Not_Found with "Controller not found: " & By_License;
   end Delete_Controller;

   procedure Add_Flight (Identifier, Origin_Airport_Name,
                        Dest_Airport_Name : String) is
      New_Flight : Flight_Record;
   begin
      for F of Flights loop
         if To_String (F.Identifier) = Identifier then
            raise Duplicate_Record with "Flight already exists: " &
                                        Identifier;
         end if;
      end loop;

      New_Flight.Identifier := To_Unbounded_String (Identifier);
      New_Flight.Origin_Name := To_Unbounded_String (Origin_Airport_Name);
      New_Flight.Destination_Name := To_Unbounded_String (Dest_Airport_Name);
      Flights.Append (New_Flight);
   end Add_Flight;

   function List_Flights return Flight_Types.Flight_Vectors.Vector is
   begin
      return Flights;
   end List_Flights;

   procedure Update_Flight (Old_Identifier, New_Origin_Name,
                           New_Dest_Name : String) is
   begin
      for I in Flights.First_Index .. Flights.Last_Index loop
         if To_String (Flights.Element (I).Identifier) = Old_Identifier then
            declare
               Updated : Flight_Record := Flights.Element (I);
            begin
               Updated.Origin_Name := To_Unbounded_String (New_Origin_Name);
               Updated.Destination_Name :=
                  To_Unbounded_String (New_Dest_Name);
               Flights.Replace_Element (I, Updated);
               return;
            end;
         end if;
      end loop;
      raise Record_Not_Found with "Flight not found: " & Old_Identifier;
   end Update_Flight;

   procedure Delete_Flight (By_Identifier : String) is
   begin
      for I in Flights.First_Index .. Flights.Last_Index loop
         if To_String (Flights.Element (I).Identifier) = By_Identifier then
            Flights.Delete (I);
            return;
         end if;
      end loop;
      raise Record_Not_Found with "Flight not found: " & By_Identifier;
   end Delete_Flight;

   procedure Get_Database_Statistics is
   begin
      Ada.Text_IO.Put_Line ("--- Statistics ---");
      Ada.Text_IO.Put_Line ("Airports:" & Airports.Length'Image);
      Ada.Text_IO.Put_Line ("Controllers:" & Controllers.Length'Image);
      Ada.Text_IO.Put_Line ("Flights:" & Flights.Length'Image);
      if DB_Connected then
         Ada.Text_IO.Put_Line ("Database: PostgreSQL available " &
                               "(flight_user@flight_management)");
         Ada.Text_IO.Put_Line ("Schema: UUID with stored functions");
         Ada.Text_IO.Put_Line ("Functions: add_*, list_*, " &
                               "update_*, delete_*");
      else
         Ada.Text_IO.Put_Line ("Database: File storage only");
      end if;
   end Get_Database_Statistics;

end Database_Operations;
