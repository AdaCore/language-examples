with Concrete_Observers;

--  The "Elaborate_Body" aspect is a boolean aspect (so, if you don't
--  mention it at all, it's set False, and if you do mention it without
--  an associated value, it's set True). It forces elaboration of
--  the package body and therefore makes the existence of the package
--  body a requirement. We use it here because the package body for
--  Actual_Observers contains a statement region that is responsible
--  for initializing the variables we declare.

package Actual_Observers with
   Elaborate_Body
is

   --  The three observers declared below are initialized in the body.

   --  If we were to declare these variables within the scope of our
   --  Main procedure, we'd run afoul of accessibility rules that
   --  compare the lifetime of the access type with the lifetime of the
   --  object declaration. Because the object declaration in the Main
   --  procedure would have a shorter lifetime than the lifetime of the
   --  access type declaration, Ada will conservatively disallow the use
   --  of 'Access because of the possibility of a dangling pointer as
   --  soon as the Main procedure goes out of scope.

   Alice : aliased Concrete_Observers.Named_Observer;
   Bob   : aliased Concrete_Observers.Named_Observer;
   Eve   : aliased Concrete_Observers.Named_Observer;

end Actual_Observers;
