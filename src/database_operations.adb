pragma Ada_2012;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Flight_Types; use Flight_Types;

package body Database_Operations is

   Airports    : Airport_Vectors.Vector;
   Controllers : Controller_Vectors.Vector;
   Flights     : Flight_Vectors.Vector;

   procedure Initialize_Database_Connection is
   begin
      Ada.Text_IO.Put_Line ("Database connection initialized (in-memory).");
   end Initialize_Database_Connection;

   procedure Shutdown_Database_Connection is
   begin
      Ada.Text_IO.Put_Line ("Database connections closed.");
   end Shutdown_Database_Connection;

   procedure Clear_All_Data is
   begin
      Airports.Clear;
      Controllers.Clear;
      Flights.Clear;
      Ada.Text_IO.Put_Line ("All data cleared.");
   end Clear_All_Data;

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
               Updated.Destination_Name := To_Unbounded_String (New_Dest_Name);
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
   end Get_Database_Statistics;

end Database_Operations;
