// 738. Problem calling correct super constructor
// In the following sample, the compiler generates a super() call to System.Object ctor, instead of the parent (Menu) one
// This causes the aChildren ivar to not be already initialized when it is used (it is being done in the Menu's ctor)

FUNCTION Start() AS VOID
ChildMenu{}

CLASS ChildMenu INHERIT Menu
CONSTRUCTOR(a,b)
// wrong ctor used here
SUPER()
// Unhandled Exception: XSharp.Error: Value cannot be null. ---> System.ArgumentNullException: Value cannot be null.
AAdd(SELF:aChildren,Menu{})
END CLASS

