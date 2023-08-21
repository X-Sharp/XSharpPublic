// 394. Ignore for now, also vulcan has incorrect(?) behavior, in this file ggg is mapped to the 
// GLOBAL in the other file in both x# and vulcan
// In a similar secnario in the RP code (need to find if the GLOBAL there (rpLKEY) is defined on
// purpose or accidentally 2 times), there is a warning in x# about an ambiguity between the GLOBALs
// but did not manage to reproduce it here. Actually it is a good warning...(but not the one shown here)

// note: there's a mess in RP, there are GLOBAL and STATIC GLOBAL sDrawText, sParaProp, sChrProp and more
// how on earth does that code work properly at runtime...

// warnings as errors so it doesn't show as test passed
STATIC GLOBAL ggg AS INT

FUNCTION Start() AS VOID
AssignToNonStatic()
? "must be zero", ggg
ggg := 2
? "must be two", ggg

