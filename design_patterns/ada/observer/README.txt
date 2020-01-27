================================================================================

This is an implementation of the Observer design pattern written in Ada.
It is also known as Dependents, and Publish-Subscribe.

--------------------------------------------------------------------------------

The comments contain lots of thoughts about how Ada features are used,
but do not really discuss the operation of the pattern itself.

For more information about the design pattern, suitable texts include:

   Design Patterns: Elements of Reusable Object-Oriented Software
   Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides

   Head First Design Patterns
   Eric Freeman, Elisabeth Robson

--------------------------------------------------------------------------------

As for the organization of the code in packages, we've got the
following, from the bottom up:

 * packages Subject and Observer:
   These contain the abstract definitions of the design patterns, as
   described in texts.

 * packages Concrete_Subjects and Concrete_Observers:
   These packages contain concrete definitions of observers and subjects
   that actually hold data and that can actually be called.

 * packages Actual_Subjects and Actual_Observers:
   These packages contain objects (in the form of variable declarations).

 * procedure Main:
   Connects the actual objects together then uses the mechanism.

--------------------------------------------------------------------------------
