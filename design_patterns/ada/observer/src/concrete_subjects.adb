with Observer; use Observer;

package body Concrete_Subjects is

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Watched_Integer; Item : in Integer) is
   begin
      --  While inside this helper procedure, we have full visibility
      --  of the private part of any Watched_Integer object. So, we are
      --  allowed to dig into the contents of the Self formal argument.
      --
      --  In other words, clients of this package do not have enough
      --  visibility to perform the simple assignment below, but the
      --  implementation of the package does:
      Self.Data := Item;
   end Set;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self : in out Watched_Integer; Item : Observer.Access_Observer)
   is
      Registered : Boolean := False;
   begin
      --  Scan our array and look for an empty slot. If we find an empty
      --  slot, assign it and set the Registered flag for later.
      for Slot of Self.Observers loop
         if Slot = null then
            Slot       := Item;
            Registered := True;
            exit;
         end if;
      end loop;

      --  If the Registered flag is not set, that means we couldn't find
      --  an empty slot. Raise an error.
      if not Registered then
         raise Constraint_Error;
      end if;
   end Register;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in out Watched_Integer; Item : Observer.Access_Observer)
   is
   begin
      --  Scan the array looking for all occurances of the given
      --  Observer. Clobber any matches.
      for Slot of Self.Observers loop
         --  Compare pointers:
         if Slot = Item then
            Slot := null;
         end if;
      end loop;
   end Remove;

   ----------------------
   -- Notify_Observers --
   ----------------------

   procedure Notify_Observers (Self : Watched_Integer) is
   begin

      --  Any non-null element of this array will hold a valid access
      --  value that points to an object of type Observer'Class. Feeling
      --  rusty? Read on.
      for Slot of Self.Observers loop

         if Slot /= null then
            --
            --  Slot is of type Access_Observer.
            --
            --  type Access_Observer is access all Observer'Class. The
            --  objects being pointed to are of a classwide type.
            --
            --  Therefore, Slot may be pointing to objects of all
            --  kinds of types as long as they are classified as
            --  being Observers (in the derivation class rooted at
            --  Observer.Observer). In this example code, we do keep
            --  things simple: we only derive once and our concrete
            --  type is called Named_Observer, but large hierarchies are
            --  possible, and extensions require zero change in existing
            --  code.
            --
            --  Because we're working with a classwide type and the
            --  specific concrete type of Slot.all is only known
            --  at run-time (let's pretend we have more than just
            --  Named_Observer's to work with), the call to Update is
            --  dispatching based on the specific concrete type of Self.
            --
            Slot.Update (Self.Data);
         end if;

      end loop;
   end Notify_Observers;

end Concrete_Subjects;
