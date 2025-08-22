pragma Ada_2012;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Flight_Types is

   type Airport_Record is record
      Name         : Ada.Strings.Unbounded.Unbounded_String;
      Location     : Ada.Strings.Unbounded.Unbounded_String;
      Max_Capacity : Positive;
   end record;

   function Image (Item : Airport_Record) return String;

   type Controller_Record is record
      License_Number   : Ada.Strings.Unbounded.Unbounded_String;
      Name             : Ada.Strings.Unbounded.Unbounded_String;
      Experience_Years : Natural;
   end record;

   function Image (Item : Controller_Record) return String;

   type Flight_Record is record
      Identifier       : Ada.Strings.Unbounded.Unbounded_String;
      Origin_Name      : Ada.Strings.Unbounded.Unbounded_String;
      Destination_Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Image (Item : Flight_Record) return String;

   package Airport_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Airport_Record);

   package Controller_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Controller_Record);

   package Flight_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Flight_Record);

end Flight_Types;
