package Observer is

   type Observer is interface;
   --  Here we have an Ada 2005 interface. An interface may be used to
   --  attach a set of subprogram specs to any tagged type, and is how
   --  we do multiple inheritance in Ada.

   type Access_Observer is access all Observer'Class;
   --  Here's a classwide general access type that we use in the
   --  primitive subprograms accompanying type Subject in package
   --  Subject. Let's unpack this comment!
   --
   --  When we say Observer'Class, we mean to be compatible with any
   --  object of any type derived from Observer.
   --
   --  You can view this type as describing a pointer that is allowed
   --  to point to objects of type Observer'Class (Observer'Class is
   --  the "designated subtype").
   --
   --  And finally, being a general access type ("access all" vs.
   --  "access") it is legal to point to objects on the heap or on
   --  the stack (dynamically allocated or declared as variables,
   --  respectively). If we left out the "all" this would instead be
   --  a pool-specific access type, and it would no longer be legal to
   --  refer to Alice, Bob, and Eve who are declared as variables in
   --  package Actual_Observers .
   --
   --  This is the type we pass in the subprograms in package Subject.
   --  The desired outcome: any type derived from Subject.Subject can
   --  work with any type derived from Observer.Observer even though
   --  there are no explicit references to specific concrete names in
   --  any of the observer/subject-related packages. It's only when you
   --  get to main.adb that everything is wired together concretely.

   --
   --  The primitive subprograms to type Observer are below. They
   --  will be inherited when types are derived from type Observer.
   --  The reason they are primitives is because all the following
   --  conditions are true:
   --
   --     * They are located within the same package as type Observer
   --
   --     * type Observer is a tagged type (interfaces are tagged types)
   --
   --     * Each subprogram directly references type Observer as part of
   --       its parameter and result profile
   --

   not overriding procedure Update
     (Self : Observer; Data : Integer) is abstract;
   --  Data could be much more interesting, but, for now let's keep it
   --  really simple.
   --
   --  As far as construction of the Observer design pattern is
   --  concerned, at this point, the Integer type is now locked in
   --  as the kind of thing that we are observing.

end Observer;
