pragma Ada_2012;
with Ada.Strings.Unbounded;

package body Flight_Types is

   -- Provides a string representation for an Airport_Record.
   function Image(Item : Airport_Record) return String is
   begin
      return "- " & Ada.Strings.Unbounded.To_String(Item.Name) &
             ", " & Ada.Strings.Unbounded.To_String(Item.Location) &
             " (Cap: " & Positive'Image(Item.Max_Capacity) & ")";
   end Image;

   -- Provides a string representation for a Controller_Record.
   function Image(Item : Controller_Record) return String is
   begin
      return "- " & Ada.Strings.Unbounded.To_String(Item.Name) &
             " (Lic: " & Ada.Strings.Unbounded.To_String(Item.License_Number) &
             ", Exp: " & Natural'Image(Item.Experience_Years) & " yrs)";
   end Image;

   -- Provides a string representation for a Flight_Record.
   function Image(Item : Flight_Record) return String is
   begin
      return "- " & Ada.Strings.Unbounded.To_String(Item.Identifier) &
             ": From " & Ada.Strings.Unbounded.To_String(Item.Origin_Name) &
             " to " & Ada.Strings.Unbounded.To_String(Item.Destination_Name);
   end Image;

end Flight_Types;
