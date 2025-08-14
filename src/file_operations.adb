pragma Ada_2012;
with Ada.Text_IO;
with Ada.Directories;
package body File_Operations is
   procedure Create_Path_If_Not_Exists(Path : String) is
   begin
      if not Ada.Directories.Exists(Path) then
         Ada.Directories.Create_Path(Path);
      end if;
   exception
      when Ada.Directories.Name_Error =>
         null; -- Path already exists, ignore.
      when others =>
         raise File_IO_Error with "Could not create directory: " & Path;
   end Create_Path_If_Not_Exists;

   procedure Save_Text(Path : String; Filename : String; Content : String) is
      File : Ada.Text_IO.File_Type;
   begin
      Create_Path_If_Not_Exists(Path);
      Ada.Text_IO.Create(File, Ada.Text_IO.Out_File, Ada.Directories.Compose(Path, Filename));
      Ada.Text_IO.Put(File, Content);
      Ada.Text_IO.Close(File);
   exception
      when others =>
         if Ada.Text_IO.Is_Open(File) then
            Ada.Text_IO.Close(File);
         end if;
         raise File_IO_Error with "Failed to save file: " & Filename;
   end Save_Text;
end File_Operations;
