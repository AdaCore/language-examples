with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package body Concrete_Observers is

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : in out Named_Observer; Name : String) is
   begin
      Self.Name := To_Unbounded_String (Name);
   end Set_Name;

   ------------
   -- Update --
   ------------

   procedure Update (Self : Named_Observer; Data : Integer) is
   begin
      Put_Line
        (To_String (Self.Name) & " notified of a: " & Data'Image);
   end Update;

end Concrete_Observers;
