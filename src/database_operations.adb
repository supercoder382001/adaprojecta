pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with ADO.Sessions;
with ADO.Properties;
with ADO.Statements;
with ADO.Queries;
with ADO.SQL;
with Flight_Types;

package body Database_Operations is

   DB_Factory : ADO.Sessions.Factory.Session_Factory;

   procedure Initialize_Database_Connection is
      Props : ADO.Properties.Manager;
   begin
      Props.Load ("src/config/database.properties");
      ADO.Sessions.Factory.Create (DB_Factory, Props);
   exception
      when E : others =>
         raise Database_Error with "DB Factory Init Failed: " &
           Ada.Exceptions.Exception_Message (E);
   end Initialize_Database_Connection;

   procedure Shutdown_Database_Connection is
   begin
      Put_Line ("Database connections closed.");
   end Shutdown_Database_Connection;

   function Get_Session return ADO.Sessions.Session is
   begin
      return DB_Factory.Get_Session;
   end Get_Session;

   procedure Call_DB_Function (SQL : String; Params : ADO.SQL.Params_List) is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session;
      Query   : ADO.Queries.Query;
   begin
      Session.Create_Query (SQL, Query);
      Query.Bind_Params (Params);
      Query.Execute;
      if Query.Is_Empty then
         raise Database_Error with "Function returned no value.";
      end if;
      if not Query.Get_Boolean then
         raise Record_Not_Found;
      end if;
   exception
      when ADO.SQL.SQL_Error =>
         raise Invalid_Input with 
           Ada.Exceptions.Exception_Message (ADO.SQL.SQL_Error'Identity);
      when E : others =>
         raise Database_Error with "Function call failed: " &
           Ada.Exceptions.Exception_Message (E);
   end Call_DB_Function;

   procedure Clear_All_Data is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session;
      Stmt    : ADO.Statements.Statement;
      SQL     : constant String := 
        "TRUNCATE TABLE airports, controllers, flights CASCADE;";
   begin
      Session.Create_Statement (SQL, Stmt);
      Stmt.Execute;
   exception
      when E : others =>
         raise Database_Error with "Failed to clear all data: " &
           Ada.Exceptions.Exception_Message (E);
   end Clear_All_Data;

   procedure Add_Airport (Name, Location : String; Max_Capacity : Positive) is
   begin
      Call_DB_Function ("SELECT add_airport(?,?,?)",
                        ADO.SQL.Create_Params (Name, Location, Max_Capacity));
   end Add_Airport;

   procedure Update_Airport (Old_Name, New_Name, New_Location : String;
                            New_Capacity : Positive) is
   begin
      Call_DB_Function ("SELECT update_airport(?,?,?,?)",
                        ADO.SQL.Create_Params (Old_Name, New_Name, 
                                              New_Location, New_Capacity));
   end Update_Airport;

   procedure Delete_Airport (By_Name : String) is
   begin
      Call_DB_Function ("SELECT delete_airport(?)",
                        ADO.SQL.Create_Params (By_Name));
   end Delete_Airport;

   function List_Airports return Flight_Types.Airport_Vectors.Vector is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session;
      Query   : ADO.Queries.Query;
      Result  : Flight_Types.Airport_Vectors.Vector;
      Item    : Flight_Types.Airport_Record;
   begin
      Session.Create_Query ("SELECT * FROM list_airports()", Query);
      Query.Execute;
      while not Query.Is_Empty loop
         Item.Name := Ada.Strings.Unbounded.To_Unbounded_String 
           (Query.Get_String (1));
         Item.Location := Ada.Strings.Unbounded.To_Unbounded_String 
           (Query.Get_String (2));
         Item.Max_Capacity := Query.Get_Integer (3);
         Result.Append (Item);
         Query.Next;
      end loop;
      return Result;
   end List_Airports;

   procedure Add_Controller (License, Name : String; Experience : Natural) is
   begin
      Call_DB_Function ("SELECT add_controller(?,?,?)",
                        ADO.SQL.Create_Params (License, Name, Experience));
   end Add_Controller;

   procedure Update_Controller (Old_License, New_Name : String;
                               New_Experience : Natural) is
   begin
      Call_DB_Function ("SELECT update_controller(?,?,?)",
                        ADO.SQL.Create_Params (Old_License, New_Name, 
                                              New_Experience));
   end Update_Controller;

   procedure Delete_Controller (By_License : String) is
   begin
      Call_DB_Function ("SELECT delete_controller(?)",
                        ADO.SQL.Create_Params (By_License));
   end Delete_Controller;

   function List_Controllers return Flight_Types.Controller_Vectors.Vector is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session;
      Query   : ADO.Queries.Query;
      Result  : Flight_Types.Controller_Vectors.Vector;
      Item    : Flight_Types.Controller_Record;
   begin
      Session.Create_Query ("SELECT * FROM list_controllers()", Query);
      Query.Execute;
      while not Query.Is_Empty loop
         Item.License_Number := Ada.Strings.Unbounded.To_Unbounded_String 
           (Query.Get_String (1));
         Item.Name := Ada.Strings.Unbounded.To_Unbounded_String 
           (Query.Get_String (2));
         Item.Experience_Years := Query.Get_Natural (3);
         Result.Append (Item);
         Query.Next;
      end loop;
      return Result;
   end List_Controllers;

   procedure Add_Flight (Identifier, Origin_Airport_Name, 
                        Dest_Airport_Name : String) is
   begin
      Call_DB_Function ("SELECT add_flight(?,?,?)",
                        ADO.SQL.Create_Params (Identifier, Origin_Airport_Name,
                                              Dest_Airport_Name));
   end Add_Flight;

   procedure Update_Flight (Old_Identifier, New_Origin_Name, 
                           New_Dest_Name : String) is
   begin
      Call_DB_Function ("SELECT update_flight(?,?,?)",
                        ADO.SQL.Create_Params (Old_Identifier, New_Origin_Name,
                                              New_Dest_Name));
   end Update_Flight;

   procedure Delete_Flight (By_Identifier : String) is
   begin
      Call_DB_Function ("SELECT delete_flight(?)",
                        ADO.SQL.Create_Params (By_Identifier));
   end Delete_Flight;

   function List_Flights return Flight_Types.Flight_Vectors.Vector is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session;
      Query   : ADO.Queries.Query;
      Result  : Flight_Types.Flight_Vectors.Vector;
      Item    : Flight_Types.Flight_Record;
   begin
      Session.Create_Query ("SELECT * FROM list_flights()", Query);
      Query.Execute;
      while not Query.Is_Empty loop
         Item.Identifier := Ada.Strings.Unbounded.To_Unbounded_String 
           (Query.Get_String (1));
         Item.Origin_Name := Ada.Strings.Unbounded.To_Unbounded_String 
           (Query.Get_String (2));
         Item.Destination_Name := Ada.Strings.Unbounded.To_Unbounded_String 
           (Query.Get_String (3));
         Result.Append (Item);
         Query.Next;
      end loop;
      return Result;
   end List_Flights;

   procedure Get_Database_Statistics is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session;
      Query   : ADO.Queries.Query;
   begin
      Session.Create_Query ("SELECT (SELECT COUNT(*) FROM airports), " &
                           "(SELECT COUNT(*) FROM controllers), " &
                           "(SELECT COUNT(*) FROM flights)", Query);
      Query.Execute;
      if not Query.Is_Empty then
         New_Line;
         Put_Line ("--- Statistics ---");
         Put_Line ("Airports: " & Query.Get_Long_Integer'Image);
         Put_Line ("Controllers: " & Query.Get_Long_Integer (2)'Image);
         Put_Line ("Flights: " & Query.Get_Long_Integer (3)'Image);
      end if;
   end Get_Database_Statistics;

end Database_Operations;
