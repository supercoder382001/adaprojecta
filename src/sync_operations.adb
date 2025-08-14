with Ada.Text_IO;
with Ada.Directories;
with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Database_Operations; use Database_Operations;
with Flight_Types;        use Flight_Types;

-- Corrected dependencies for the JSON-Ada library
with JSON.IO;
with JSON.Values;

package body Sync_Operations is

   -- This is the corrected procedure using the JSON-Ada library
   procedure Export_All_To_JSON is
      use Ada.Calendar;
      use JSON.Values;

      -- Procedure to convert an airport to a JSON object
      function To_Json(A : Airport_Record) return JSON_Value is
         Obj : JSON_Object_Access := Create_Object;
      begin
         Obj.Set("name", To_String(A.Name));
         Obj.Set("location", To_String(A.Location));
         Obj.Set("capacity", Integer(A.Max_Capacity));
         return To_JSON_Value(Obj);
      end To_Json;

      -- Procedure to convert a controller to a JSON object
      function To_Json(C : Controller_Record) return JSON_Value is
         Obj : JSON_Object_Access := Create_Object;
      begin
         Obj.Set("name", To_String(C.Name));
         Obj.Set("contact", To_String(C.Contact_Info));
         return To_JSON_Value(Obj);
      end To_Json;

      -- Procedure to convert a flight to a JSON object
      function To_Json(F : Flight_Record) return JSON_Value is
         Obj : JSON_Object_Access := Create_Object;
      begin
         Obj.Set("flight_number", To_String(F.Flight_Number));
         Obj.Set("origin", To_String(F.Origin));
         Obj.Set("destination", To_String(F.Destination));
         Obj.Set("status", To_String(F.Status));
         return To_JSON_Value(Obj);
      end To_Json;

      -- Main export logic
      Root_Object   : JSON_Object_Access := Create_Object;
      Airports_Array : JSON_Array_Access  := Create_Array;
      Controllers_Array : JSON_Array_Access := Create_Array;
      Flights_Array     : JSON_Array_Access := Create_Array;

      Data_Path : constant String := "data/exports/";
      Timestamp : constant String := Time_Of(Now)'Image;
      Clean_Timestamp : String := Timestamp;
      Filename      : Unbounded_String;

   begin
      -- Fetch data from database
      for A of Get_All_Airports loop
         Airports_Array.Append(To_Json(A));
      end loop;

      for C of Get_All_Controllers loop
         Controllers_Array.Append(To_Json(C));
      end loop;

      for F of Get_All_Flights loop
         Flights_Array.Append(To_Json(F));
      end loop;

      -- Assemble the root JSON object
      Root_Object.Set("airports", To_JSON_Value(Airports_Array));
      Root_Object.Set("controllers", To_JSON_Value(Controllers_Array));
      Root_Object.Set("flights", To_JSON_Value(Flights_Array));

      -- Create filename
      for I in Clean_Timestamp'Range loop
         if Clean_Timestamp(I) = ':' or Clean_Timestamp(I) = ' ' then
            Clean_Timestamp(I) := '-';
         end if;
      end loop;

      if not Ada.Directories.Exists(Data_Path) then
         Ada.Directories.Create_Path(Data_Path);
      end if;

      Filename := To_Unbounded_String(Data_Path & "export-" & Clean_Timestamp & ".json");
      JSON.IO.Write(Root_Object.all, To_String(Filename));

      Ada.Text_IO.Put_Line("SUCCESS: Data exported to " & To_String(Filename));

   exception
      when others =>
         Ada.Text_IO.Put_Line("ERROR: Failed to export data to JSON.");

   end Export_All_To_JSON;

   procedure Import_All_From_JSON (Filename : in String) is
   begin
      -- Implementation for importing data would go here.
      -- This is left as an exercise.
      Ada.Text_IO.Put_Line ("INFO: JSON import not yet implemented.");
   end Import_All_From_JSON;

end Sync_Operations;
