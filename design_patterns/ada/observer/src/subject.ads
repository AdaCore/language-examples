with Observer;

package Subject is

   type Subject is interface;
   --  Here we have an Ada 2005 interface. An interface may be used to
   --  attach a set of subprogram specs to any tagged type, and is how
   --  we do multiple inheritance in Ada.

   --
   --  The primitive subprograms to type Subject are below. They will
   --  be inherited when types are derived from type Subject. The reason
   --  they are primitives is because all the following conditions are
   --  true:
   --
   --     * They are located within the same package as type Subject
   --
   --     * type Subject is a tagged type (interfaces are tagged types)
   --
   --     * Each subprogram directly references type Subject as part of
   --       its parameter and result profile
   --

   not overriding procedure Register
     (Self : in out Subject;
      Item :        Observer.Access_Observer) is abstract;

   not overriding procedure Remove
     (Self : in out Subject;
      Item :        Observer.Access_Observer) is abstract;

   not overriding procedure Notify_Observers
     (Self : Subject) is abstract;

end Subject;
