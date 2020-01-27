with Ada.Strings.Unbounded;
with Observer;

package Concrete_Observers is

   type Named_Observer is new Observer.Observer with record
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Declare a type Named_Observer: it's a tagged record that includes
   --  the Observer interface.
   --
   --  A named observer is just that: it's an observer that stores some
   --  name data that allows it to self-identify.
   --
   --  The Observer interface, being an interface, can not have any
   --  record components, so none are inherited. Objects of this type
   --  will therefore contain only the one record component declared
   --  here.
   --
   --  We really should declare this with a private record
   --  extension so that we can maintain some abstraction (see
   --  Concrete_Subjects.Watched_Integer for an example of a private
   --  record extension). However, so that we can illustrate a simple
   --  record extension in this demo, we'll keep this as-is.

   overriding procedure Update (Self : Named_Observer; Data : Integer);
   --  Override the primitive subprogram that comes with the Observer
   --  interface with a local implementation. Especially because the
   --  inherited subprogram is abstract (and unimplemented), it is
   --  necessary to provide some actual implementation if we want
   --  to call the procedure at all.
   --
   --  Using the "overriding" keyword is optional. But, by using it here
   --  we inform the compiler that we intend for this to override an
   --  inherited primitive. If that's not the case then the compilation
   --  will fail.

   not overriding procedure Set_Name
     (Self : in out Named_Observer; Name : String);
   --  Set the name for our named observer.
   --
   --  This is a new primitive for all types derived from
   --  Named_Observer.
   --
   --  Again, we use the optional feature and say "not overriding" which
   --  causes the compiler to bomb if the subprogram is inherited.

end Concrete_Observers;
