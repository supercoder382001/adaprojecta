pragma Ada_2012;

package Test_Runner is

   --  Main procedure to run all test cases from file
   procedure Handle_Run_Test_Cases;

   --  Individual test execution procedures
   procedure Execute_Add_Airport_Test
     (Name, Location : String; Capacity : Positive);
   procedure Execute_Add_Controller_Test
     (License, Name : String; Experience : Natural);
   procedure Execute_Add_Flight_Test
     (Identifier, Origin, Destination : String);
   procedure Execute_Delete_Test 
     (Entity_Type, Identifier : String);
   procedure Execute_Verify_Count_Test
     (Entity_Type : String; Expected_Count : Natural);

   --  Utility procedures
   function Parse_Test_Line 
     (Line : String; Length : Natural) return Boolean;
   procedure Show_Test_Results 
     (Total_Tests, Passed_Tests : Natural);
   procedure Log_Test_Result 
     (Success : Boolean; Message : String);

end Test_Runner;
