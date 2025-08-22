pragma Ada_2012;
package File_Operations is
   File_IO_Error : exception;
   procedure Create_Path_If_Not_Exists(Path : String);
   procedure Save_Text(Path : String; Filename : String; Content : String);
end File_Operations;
