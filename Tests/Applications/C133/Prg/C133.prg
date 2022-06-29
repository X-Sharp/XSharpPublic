// 133. No warning on not using assigned value
#pragma warnings(219, off) // assigned but never used
FUNCTION Start() AS VOID
LOCAL n := 1 AS INT // correct warning XS0219 here
LOCAL o := System.Collections.ArrayList{} AS OBJECT // no warning


/*
This "not used" for the object o is not reported on purpose by the compiler:
============================================================================
// A local variable that is written to is considered to also be read,
// unless the written value is always a constant. The reasons for this
// unusual behavior are:
//
// * The debugger does not make it easy to see the returned value of
//   a method. Often a call whose returned value would normally be
//   discarded is written into a local variable so that it can be
//   easily inspected in the debugger.
//
// * An otherwise unread local variable that contains a reference to an
//   object can keep the object alive longer, particularly if the jitter
//   is not optimizing the lifetimes of locals. (Because, for example,
//   the debugger is running.) Again, this can be useful when debugging
//   because an otherwise unused object might be finalized later, allowing
//   the developer to more easily examine its state.
//
// * A developer who wishes to deliberately discard a value returned by
//   a method can do so in a self-documenting manner via
//   "var unread = M();"
//
// We suppress the "written but not read" message on locals unless what is
// written is a constant, a null, a default(T) expression, a default constructor
// of a value type, or a built-in conversion operating on a constant, etc.

*/
