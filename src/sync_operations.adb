pragma Ada_2012;

with Ada.Text_IO;
with Ada.Directories;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Database_Operations; use Database_Operations;
with Flight_Types; use Flight_Types;

package body Sync_Operations is

   function To_JSON_String (A : Airport_Record) return String;
   function To_JSON_String (C : Controller_Record) return String;
   function To_JSON_String (F : Flight_Record) return String;

   function To_JSON_String (A : Airport_Record) return String is
   begin
      return "    {" & ASCII.LF &
             "      ""name"": """ & To_String (A.Name) & """," & ASCII.LF &
             "      ""location"": """ & To_String (A.Location) &
             """," & ASCII.LF &
             "      ""max_capacity"": " & A.Max_Capacity'Image & ASCII.LF &
             "    }";
   end To_JSON_String;

   function To_JSON_String (C : Controller_Record) return String is
   begin
      return "    {" & ASCII.LF &
             "      ""license"": """ & To_String (C.License_Number) &
             """," & ASCII.LF &
             "      ""name"": """ & To_String (C.Name) &
             """," & ASCII.LF &
             "      ""experience_years"": " & C.Experience_Years'Image &
             ASCII.LF &
             "    }";
   end To_JSON_String;

   function To_JSON_String (F : Flight_Record) return String is
   begin
      return "    {" & ASCII.LF &
             "      ""identifier"": """ & To_String (F.Identifier) &
             """," & ASCII.LF &
             "      ""origin"": """ & To_String (F.Origin_Name) &
             """," & ASCII.LF &
             "      ""destination"": """ & To_String (F.Destination_Name) &
             """" & ASCII.LF &
             "    }";
   end To_JSON_String;

   procedure Export_All_To_JSON is
      Data_Path       : constant String := "data/exports/";
      Timestamp       : constant String :=
        Ada.Calendar.Formatting.Image (Ada.Calendar.Clock);
      Clean_Timestamp : String := Timestamp;
      Filename        : Unbounded_String;
      Output_File     : Ada.Text_IO.File_Type;

      Airports    : constant Airport_Vectors.Vector := List_Airports;
      Controllers : constant Controller_Vectors.Vector := List_Controllers;
      Flights     : constant Flight_Vectors.Vector := List_Flights;

   begin
      for I in Clean_Timestamp'Range loop
         if Clean_Timestamp (I) = ':' or Clean_Timestamp (I) = ' ' or
            Clean_Timestamp (I) = '.' then
            Clean_Timestamp (I) := '-';
         end if;
      end loop;

      if not Ada.Directories.Exists (Data_Path) then
         Ada.Directories.Create_Path (Data_Path);
      end if;

      Filename := To_Unbounded_String (Data_Path & "export-" &
                                       Clean_Timestamp & ".json");

      Ada.Text_IO.Create (Output_File, Ada.Text_IO.Out_File,
                          To_String (Filename));

      Ada.Text_IO.Put_Line (Output_File, "{");

      Ada.Text_IO.Put_Line (Output_File, "  ""airports"": [");
      for I in Airports.First_Index .. Airports.Last_Index loop
         Ada.Text_IO.Put (Output_File,
                          To_JSON_String (Airports.Element (I)));
         if I < Airports.Last_Index then
            Ada.Text_IO.Put_Line (Output_File, ",");
         else
            Ada.Text_IO.New_Line (Output_File);
         end if;
      end loop;
      Ada.Text_IO.Put_Line (Output_File, "  ],");

      Ada.Text_IO.Put_Line (Output_File, "  ""controllers"": [");
      for I in Controllers.First_Index .. Controllers.Last_Index loop
         Ada.Text_IO.Put (Output_File,
                          To_JSON_String (Controllers.Element (I)));
         if I < Controllers.Last_Index then
            Ada.Text_IO.Put_Line (Output_File, ",");
         else
            Ada.Text_IO.New_Line (Output_File);
         end if;
      end loop;
      Ada.Text_IO.Put_Line (Output_File, "  ],");

      Ada.Text_IO.Put_Line (Output_File, "  ""flights"": [");
      for I in Flights.First_Index .. Flights.Last_Index loop
         Ada.Text_IO.Put (Output_File,
                          To_JSON_String (Flights.Element (I)));
         if I < Flights.Last_Index then
            Ada.Text_IO.Put_Line (Output_File, ",");
         else
            Ada.Text_IO.New_Line (Output_File);
         end if;
      end loop;
      Ada.Text_IO.Put_Line (Output_File, "  ]");

      Ada.Text_IO.Put_Line (Output_File, "}");
      Ada.Text_IO.Close (Output_File);

      Ada.Text_IO.Put_Line ("SUCCESS: Data exported to " &
                            To_String (Filename));

   exception
      when others =>
         if Ada.Text_IO.Is_Open (Output_File) then
            Ada.Text_IO.Close (Output_File);
         end if;
         raise Sync_Error with "Failed to export data to JSON";
   end Export_All_To_JSON;

   procedure Import_All_From_JSON (Filename : in String) is
      Input_File : Ada.Text_IO.File_Type;
   begin
      if not Ada.Directories.Exists (Filename) then
         raise Sync_Error with "File not found: " & Filename;
      end if;

      Ada.Text_IO.Open (Input_File, Ada.Text_IO.In_File, Filename);

      Ada.Text_IO.Put_Line ("INFO: Reading JSON file: " & Filename);
      while not Ada.Text_IO.End_Of_File (Input_File) loop
         Ada.Text_IO.Put_Line ("  " & Ada.Text_IO.Get_Line (Input_File));
      end loop;

      Ada.Text_IO.Close (Input_File);
      Ada.Text_IO.Put_Line ("INFO: JSON import parsing not yet " &
                            "implemented");

   exception
      when others =>
         if Ada.Text_IO.Is_Open (Input_File) then
            Ada.Text_IO.Close (Input_File);
         end if;
         raise Sync_Error with "Failed to import data from JSON file: " &
                               Filename;
   end Import_All_From_JSON;

   procedure Initialize_Sync_Config is
      Config_Path : constant String := "config/";
   begin
      if not Ada.Directories.Exists (Config_Path) then
         Ada.Directories.Create_Path (Config_Path);
         Ada.Text_IO.Put_Line ("Created config directory: " & Config_Path);
      end if;

      if not Ada.Directories.Exists ("data/") then
         Ada.Directories.Create_Path ("data/");
         Ada.Text_IO.Put_Line ("Created data directory: data/");
      end if;

      if not Ada.Directories.Exists ("data/backups/") then
         Ada.Directories.Create_Path ("data/backups/");
         Ada.Text_IO.Put_Line ("Created backups directory: data/backups/");
      end if;

      Ada.Text_IO.Put_Line ("Sync configuration initialized successfully.");
   end Initialize_Sync_Config;

   procedure Create_Full_Backup is
      Timestamp       : constant String :=
        Ada.Calendar.Formatting.Image (Ada.Calendar.Clock);
      Clean_Timestamp : String := Timestamp;
      Backup_Path     : constant String := "data/backups/";
      Backup_File     : Unbounded_String;
      Output_File     : Ada.Text_IO.File_Type;

      Airports    : constant Airport_Vectors.Vector := List_Airports;
      Controllers : constant Controller_Vectors.Vector := List_Controllers;
      Flights     : constant Flight_Vectors.Vector := List_Flights;

   begin
      for I in Clean_Timestamp'Range loop
         if Clean_Timestamp (I) = ':' or Clean_Timestamp (I) = ' ' or
            Clean_Timestamp (I) = '.' then
            Clean_Timestamp (I) := '-';
         end if;
      end loop;

      if not Ada.Directories.Exists (Backup_Path) then
         Ada.Directories.Create_Path (Backup_Path);
      end if;

      Backup_File := To_Unbounded_String (Backup_Path & "backup-" &
                                          Clean_Timestamp & ".txt");

      Ada.Text_IO.Create (Output_File, Ada.Text_IO.Out_File,
                          To_String (Backup_File));

      Ada.Text_IO.Put_Line (Output_File,
                            "========================================");
      Ada.Text_IO.Put_Line (Output_File, "FLIGHT MANAGEMENT SYSTEM BACKUP");
      Ada.Text_IO.Put_Line (Output_File, "Generated: " & Timestamp);
      Ada.Text_IO.Put_Line (Output_File,
                            "========================================");
      Ada.Text_IO.New_Line (Output_File);

      Ada.Text_IO.Put_Line (Output_File, "AIRPORTS (" &
                            Airports.Length'Image & " records):");
      Ada.Text_IO.Put_Line (Output_File,
                            "----------------------------------------");
      for A of Airports loop
         Ada.Text_IO.Put_Line (Output_File,
                               "Name: " & To_String (A.Name));
         Ada.Text_IO.Put_Line (Output_File,
                               "Location: " & To_String (A.Location));
         Ada.Text_IO.Put_Line (Output_File,
                               "Capacity: " & A.Max_Capacity'Image);
         Ada.Text_IO.Put_Line (Output_File, "---");
      end loop;
      Ada.Text_IO.New_Line (Output_File);

      Ada.Text_IO.Put_Line (Output_File, "CONTROLLERS (" &
                            Controllers.Length'Image & " records):");
      Ada.Text_IO.Put_Line (Output_File,
                            "----------------------------------------");
      for C of Controllers loop
         Ada.Text_IO.Put_Line (Output_File,
                               "License: " & To_String (C.License_Number));
         Ada.Text_IO.Put_Line (Output_File,
                               "Name: " & To_String (C.Name));
         Ada.Text_IO.Put_Line (Output_File, "Experience: " &
                               C.Experience_Years'Image & " years");
         Ada.Text_IO.Put_Line (Output_File, "---");
      end loop;
      Ada.Text_IO.New_Line (Output_File);

      Ada.Text_IO.Put_Line (Output_File, "FLIGHTS (" &
                            Flights.Length'Image & " records):");
      Ada.Text_IO.Put_Line (Output_File,
                            "----------------------------------------");
      for F of Flights loop
         Ada.Text_IO.Put_Line (Output_File,
                               "Identifier: " & To_String (F.Identifier));
         Ada.Text_IO.Put_Line (Output_File,
                               "Origin: " & To_String (F.Origin_Name));
         Ada.Text_IO.Put_Line (Output_File, "Destination: " &
                               To_String (F.Destination_Name));
         Ada.Text_IO.Put_Line (Output_File, "---");
      end loop;

      Ada.Text_IO.Close (Output_File);

      Ada.Text_IO.Put_Line ("SUCCESS: Full backup created at " &
                            To_String (Backup_File));

   exception
      when others =>
         if Ada.Text_IO.Is_Open (Output_File) then
            Ada.Text_IO.Close (Output_File);
         end if;
         raise Sync_Error with "Failed to create backup";
   end Create_Full_Backup;

end Sync_Operations;
