// 957. Failed to emit module error with static calls to unknown type (#1803)
// https://github.com/X-Sharp/XSharpPublic/issues/1803

// error XS7038: Failed to emit module 'C957': Unable to determine specific cause of the failure.
#pragma options(allowdot, off)
FUNCTION Start() AS VOID
UnknownType.UnknownMethod()
? UnknownType.UnknownProperty
