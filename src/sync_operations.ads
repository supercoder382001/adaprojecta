pragma Ada_2012;

package Sync_Operations is

   Sync_Error : exception;

   procedure Export_All_To_JSON;
   procedure Import_All_From_JSON (Filename : in String);
   procedure Initialize_Sync_Config;
   procedure Create_Full_Backup;

end Sync_Operations;
