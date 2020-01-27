with Actual_Observers; use Actual_Observers;
with Actual_Subjects;  use Actual_Subjects;

procedure Main is
begin

   --
   --  Add Alice as an observer of the Watched_Integer object I.
   --
   --  We are permitted to use the "distinguished receiver" syntax,
   --  because I is an object of a tagged type, the Register subprogram
   --  is a primitive of that tagged type, and the first argument of the
   --  subprogram is also of that tagged type.
   --
   --  Because we are using this "distinguished receiver" syntax,
   --  it is also unnecessary to "with" the package that defines the
   --  type of I (it's of type Watched_Integer which is declared in
   --  Concrete_Subjects). To call procedure Register in the traditional
   --  way, we have to "with Concrete_Subjects" above and then call the
   --  procedure with:
   --
   --     Concrete_Subjects.Register (I, Alice'Access);
   --
   I.Register (Alice'Access);

   --  Add Bob as an observer of the Watched_Integer object I.
   I.Register (Bob'Access);

   --  Add, but then remove, Eve as an observer as well. Eve won't get
   --  any notifications because Eve was ultimately removed.
   I.Register (Eve'Access);
   I.Remove (Eve'Access);

   --  For demo's sake, set the value of I and notify all observers of
   --  I. Three times.
   for K in 1 .. 3 loop
      I.Set (K);
      I.Notify_Observers;
   end loop;

end Main;
