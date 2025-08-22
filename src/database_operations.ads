pragma Ada_2012;
with Flight_Types;

package Database_Operations is
   Database_Error, Record_Not_Found, Duplicate_Record, Invalid_Input :
     exception;

   procedure Initialize_Database_Connection;
   procedure Shutdown_Database_Connection;

   procedure Clear_All_Data;
   procedure Save_All_Data;
   procedure Load_All_Data;

   procedure Save_To_File;
   procedure Save_To_Database;
   procedure Load_From_File;
   procedure Load_From_Database;
   procedure Clear_Database;

   procedure Add_Airport (Name : String; Location : String;
                         Max_Capacity : Positive);
   function List_Airports return Flight_Types.Airport_Vectors.Vector;
   procedure Update_Airport (Old_Name, New_Name, New_Location : String;
                            New_Capacity : Positive);
   procedure Delete_Airport (By_Name : String);

   procedure Add_Controller (License, Name : String; Experience : Natural);
   function List_Controllers return Flight_Types.Controller_Vectors.Vector;
   procedure Update_Controller (Old_License, New_Name : String;
                               New_Experience : Natural);
   procedure Delete_Controller (By_License : String);

   procedure Add_Flight (Identifier, Origin_Airport_Name,
                        Dest_Airport_Name : String);
   function List_Flights return Flight_Types.Flight_Vectors.Vector;
   procedure Update_Flight (Old_Identifier, New_Origin_Name,
                           New_Dest_Name : String);
   procedure Delete_Flight (By_Identifier : String);

   procedure Get_Database_Statistics;
end Database_Operations;
