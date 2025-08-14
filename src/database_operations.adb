pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with ADO.Sessions; with ADO.Properties; with ADO.Statements; with ADO.Queries; with ADO.SQL;
with Flight_Types;

package body Database_Operations is
   -- (All other procedures and helpers remain the same)
   DB_Factory : ADO.Sessions.Factory.Session_Factory;
   procedure Initialize_Database_Connection is Props : ADO.Properties.Manager;
   begin Props.Load ("src/config/database.properties"); ADO.Sessions.Factory.Create (DB_Factory, Props);
   exception when E:others => raise Database_Error with "DB Factory Init Failed: " & Exception_Message(E); end;
   procedure Shutdown_Database_Connection is begin Put_Line("Database connections closed."); end;
   function Get_Session return ADO.Sessions.Session is (DB_Factory.Get_Session);
   procedure Call_DB_Function(SQL : String; Params : ADO.SQL.Params_List) is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session; Query : ADO.Queries.Query;
   begin
      Session.Create_Query(SQL, Query); Query.Bind_Params(Params); Query.Execute;
      if Query.Is_Empty then raise Database_Error with "Function returned no value."; end if;
      if not Query.Get_Boolean then raise Record_Not_Found; end if;
   exception
      when ADO.SQL.SQL_Error => raise Invalid_Input with Exception_Message(ADO.SQL.SQL_Error'Identity);
      when E : others => raise Database_Error with "Function call failed: " & Exception_Message(E);
   end Call_DB_Function;

   -- =========================================================
   -- == New Procedure Implementation
   -- =========================================================
   procedure Clear_All_Data is
      Session : ADO.Sessions.Session := DB_Factory.Get_Session;
      Stmt    : ADO.Statements.Statement;
      -- TRUNCATE is a fast way to delete all rows.
      -- CASCADE automatically handles foreign key constraints.
      SQL     : constant String := "TRUNCATE TABLE airports, controllers, flights CASCADE;";
   begin
      Session.Create_Statement(SQL, Stmt);
      Stmt.Execute;
   exception
      when E : others =>
         raise Database_Error with "Failed to clear all data: " & Exception_Message(E);
   end Clear_All_Data;

   -- (All other CRUD procedure implementations remain the same)
   procedure Add_Airport(Name, Loc : String; Cap : Positive) is begin Call_DB_Function("SELECT add_airport(?,?,?)", ADO.SQL.Create_Params(Name,Loc,Cap)); end;
   procedure Update_Airport(Old_N, New_N, New_L : String; New_C : Positive) is begin Call_DB_Function("SELECT update_airport(?,?,?,?)", ADO.SQL.Create_Params(Old_N,New_N,New_L,New_C)); end;
   procedure Delete_Airport(By_Name : String) is begin Call_DB_Function("SELECT delete_airport(?)", ADO.SQL.Create_Params(By_Name)); end;
   function List_Airports return Flight_Types.Airport_Vectors.Vector is Session:ADO.Sessions.Session:=DB_Factory.Get_Session; Query:ADO.Queries.Query; Result:Flight_Types.Airport_Vectors.Vector; Item:Flight_Types.Airport_Record; begin Session.Create_Query("SELECT * FROM list_airports()", Query); Query.Execute; while not Query.Is_Empty loop Item.Name:=Ada.Strings.Unbounded.To_Unbounded_String(Query.Get_String(1)); Item.Location:=Ada.Strings.Unbounded.To_Unbounded_String(Query.Get_String(2)); Item.Max_Capacity:=Query.Get_Integer(3); Result.Append(Item); Query.Next; end loop; return Result; end;
   procedure Add_Controller(Lic, Name : String; Exp : Natural) is begin Call_DB_Function("SELECT add_controller(?,?,?)", ADO.SQL.Create_Params(Lic,Name,Exp)); end;
   procedure Update_Controller(Old_L, New_N : String; New_E : Natural) is begin Call_DB_Function("SELECT update_controller(?,?,?)", ADO.SQL.Create_Params(Old_L,New_N,New_E)); end;
   procedure Delete_Controller(By_Lic : String) is begin Call_DB_Function("SELECT delete_controller(?)", ADO.SQL.Create_Params(By_Lic)); end;
   function List_Controllers return Flight_Types.Controller_Vectors.Vector is Session:ADO.Sessions.Session:=DB_Factory.Get_Session; Query:ADO.Queries.Query; Result:Flight_Types.Controller_Vectors.Vector; Item:Flight_Types.Controller_Record; begin Session.Create_Query("SELECT * FROM list_controllers()", Query); Query.Execute; while not Query.Is_Empty loop Item.License_Number:=Ada.Strings.Unbounded.To_Unbounded_String(Query.Get_String(1)); Item.Name:=Ada.Strings.Unbounded.To_Unbounded_String(Query.Get_String(2)); Item.Experience_Years:=Query.Get_Natural(3); Result.Append(Item); Query.Next; end loop; return Result; end;
   procedure Add_Flight(ID, Orig, Dest : String) is begin Call_DB_Function("SELECT add_flight(?,?,?)", ADO.SQL.Create_Params(ID,Orig,Dest)); end;
   procedure Update_Flight(Old_ID, New_O, New_D : String) is begin Call_DB_Function("SELECT update_flight(?,?,?)", ADO.SQL.Create_Params(Old_ID,New_O,New_D)); end;
   procedure Delete_Flight(By_ID : String) is begin Call_DB_Function("SELECT delete_flight(?)", ADO.SQL.Create_Params(By_ID)); end;
   function List_Flights return Flight_Types.Flight_Vectors.Vector is Session:ADO.Sessions.Session:=DB_Factory.Get_Session; Query:ADO.Queries.Query; Result:Flight_Types.Flight_Vectors.Vector; Item:Flight_Types.Flight_Record; begin Session.Create_Query("SELECT * FROM list_flights()", Query); Query.Execute; while not Query.Is_Empty loop Item.Identifier:=Ada.Strings.Unbounded.To_Unbounded_String(Query.Get_String(1)); Item.Origin_Name:=Ada.Strings.Unbounded.To_Unbounded_String(Query.Get_String(2)); Item.Destination_Name:=Ada.Strings.Unbounded.To_Unbounded_String(Query.Get_String(3)); Result.Append(Item); Query.Next; end loop; return Result; end;
   procedure Get_Database_Statistics is Session:ADO.Sessions.Session:=DB_Factory.Get_Session; Query:ADO.Queries.Query; begin Session.Create_Query("SELECT(SELECT COUNT(*) FROM airports),(SELECT COUNT(*) FROM controllers),(SELECT COUNT(*) FROM flights)", Query); Query.Execute; if not Query.Is_Empty then New_Line; Put_Line("--- Statistics ---"); Put_Line("Airports: "&Query.Get_Long_Integer'Image); Put_Line("Controllers: "&Query.Get_Long_Integer(2)'Image); Put_Line("Flights: "&Query.Get_Long_Integer(3)'Image); end if; end;
end Database_Operations;
