with Observer;
with Subject;

package Concrete_Subjects is

   subtype Observer_Index is Integer range 1 .. 64;
   --  Arbitrary range

   type Observer_Array is
     array (Observer_Index) of Observer.Access_Observer;
   --  A simple data structure that stores pointers to objects of
   --  a classwide general access type. Therefore, we can point
   --  to any object that is in the derivation class rooted at
   --  Observer.Observer.
   --
   --  See comments for the type Observer.Access_Observer for more info.
   --
   --  Because access objects are always initialized to null in Ada,
   --  there is no need to provide an initializer. All objects of this
   --  array type will have their elements initialized to null.

   type Watched_Integer is new Subject.Subject with private;
   --  Here is the concrete type we create using the Subject.Subject
   --  interface. It derives from an interface, but is itself a tagged
   --  record and therefore it is possible to create objects of this
   --  type.
   --
   --  We do not want clients of this package to be able to directly
   --  manipulate the contents of this record, we want to force them to
   --  go through the interface we designed. So, we make its contents
   --  private. The completion of this declaration is below after the
   --  keyword "private."
   --
   --  What can you do with a private type? You can create objects of
   --  the type, you can assign, you can compare for equality, and you
   --  can use subprograms that accept objects of the type as arguments.

   not overriding procedure Set
     (Self : in out Watched_Integer; Item : in Integer);
   --  Use this to set the value that's stored in the Watched_Integer
   --  object. This is a new primitive of our Watched_Integer tagged
   --  type.
   --
   --  We provide this procedure because we need the ability to set the
   --  value, but do not want to allow clients to directly manipulate
   --  objects of type Watched_Integer.

   --
   --  Below, we override the primitive subprograms that we inherit from
   --  the Subject.Subject interface.
   --
   --  Using the "overriding" keyword is optional. But, by using it here
   --  we inform the compiler that we intend for this to override an
   --  inherited primitive. If that's not the case then the compilation
   --  will fail.
   --

   overriding procedure Register
     (Self : in out Watched_Integer; Item : Observer.Access_Observer);

   overriding procedure Remove
     (Self : in out Watched_Integer; Item : Observer.Access_Observer);

   overriding procedure Notify_Observers (Self : Watched_Integer);

private

   --  Here is the completion for the private type Watched_Integer:
   type Watched_Integer is new Subject.Subject with record

      Observers : Observer_Array;
      --  The database of observers could have a more interesting
      --  implementation, but, we'll keep it simple for this demo.
      --  Elements of this array will either be set to point to a
      --  valid Observer, or will be null.

      Data : Integer := 0;
      --  Data could be more interesting, but, we'll keep it super
      --  simple for this demo. This data type needs to match the type
      --  accepted by the Update primitive of the Observer interface.

   end record;

end Concrete_Subjects;
