pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with ADO.Sessions; with ADO.Properties; with ADO.Queries; with GNAT.JSON;
with Database_Operations;
with File_Operations;

package body Sync_Operations is
   Export_Path : Ada.Strings.Unbounded.Unbounded_String;
   Backup_Path : Ada.Strings.Unbounded.Unbounded_String;

   procedure Initialize_Sync_Config is
      Props : ADO.Properties.Manager;
   begin
      Props.Load("src/config/sync_config.properties");
      Export_Path := Ada.Strings.Unbounded.To_Unbounded_String(Props.Get("export_path"));
      Backup_Path := Ada.Strings.Unbounded.To_Unbounded_String(Props.Get("backup_path"));
   end Initialize_Sync_Config;

   function Query_To_JSON(Query : in out ADO.Queries.Query) return String is
      JSON_Array : GNAT.JSON.JSON_Array_Type;
   begin
      while not Query.Is_Empty loop
         declare
            JSON_Object : GNAT.JSON.JSON_Object_Type;
         begin
            for I in 1 .. Query.Get_Column_Count loop
               JSON_Object.Set(Query.Get_Column_Name(I), Query.Get_String);
               Query.Next_Column;
            end loop;
            JSON_Array.Append(GNAT.JSON.Create(JSON_Object));
         end;
         Query.Next;
      end loop;
      return GNAT.JSON.Image(GNAT.JSON.Create(JSON_Array));
   end Query_To_JSON;

   procedure Export_Table(Table_Name : String; Path : String) is
      Session      : ADO.Sessions.Session;
      Query        : ADO.Queries.Query;
      JSON_Content : String;
   begin
      Session := Database_Operations.Get_Session;
      Session.Create_Query("SELECT * FROM list_" & Table_Name & "()", Query);
      Query.Execute;
      JSON_Content := Query_To_JSON(Query);
      File_Operations.Save_Text(Path, Table_Name & ".json", JSON_Content);
      Put_Line("SUCCESS: Exported '" & Table_Name & "' to " & Path);
   exception
      when E : others =>
         raise Sync_Error with "Export failed for '" & Table_Name & "': " & Exception_Message(E);
   end Export_Table;

   procedure Export_All_To_JSON is
      Path : constant String := Ada.Strings.Unbounded.To_String(Export_Path);
   begin
      New_Line; Put_Line("--- Starting Full JSON Export ---");
      Export_Table("airports", Path);
      Export_Table("controllers", Path);
      Export_Table("flights", Path);
      Put_Line("--- JSON Export Complete ---");
   end Export_All_To_JSON;

   procedure Create_Full_Backup is
      Time_Now         : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Time_Image       : constant String := Ada.Calendar.Formatting.Image(Time_Now, True);
      Safe_Timestamp   : constant String := Ada.Strings.Fixed.Translate(Time_Image, Ada.Strings.Maps.To_Mapping(" :", "__"));
      Backup_Dir       : constant String := Ada.Directories.Compose(Ada.Strings.Unbounded.To_String(Backup_Path), "backup_" & Safe_Timestamp);
      Meta_File_Content: constant String := "Backup Created: " & Time_Image;
   begin
      New_Line; Put_Line("--- Creating Full Backup ---");
      Export_Table("airports", Backup_Dir);
      Export_Table("controllers", Backup_Dir);
      Export_Table("flights", Backup_Dir);
      File_Operations.Save_Text(Backup_Dir, "backup_metadata.txt", Meta_File_Content);
      Put_Line("--- Backup Complete. Data saved to: " & Backup_Dir);
   end Create_Full_Backup;

end Sync_Operations;
