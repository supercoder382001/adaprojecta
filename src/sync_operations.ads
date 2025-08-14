pragma Ada_2012;
package Sync_Operations is
   Sync_Error : exception;
   procedure Initialize_Sync_Config;
   procedure Export_All_To_JSON;
   procedure Create_Full_Backup;
end Sync_Operations;
