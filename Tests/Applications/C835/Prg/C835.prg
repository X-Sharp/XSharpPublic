// 835. Missing warnings for signed/unsigned conversions
// https://github.com/X-Sharp/XSharpPublic/issues/971
/*
In the following code, VO reports a warning "overflow or loss of data possible converting xxx -> yyy", for all assignments, except for the ones with BYTEs.
Which is clearly inconsistent, the intended behavior probably was to report a warning in ALL cases, but VO is VO...

In X#, when /vo4- is disabled, then there's an error reported for ALL lines, which is correct.

When /vo4+ is enabled, then there's no error, which is also correct. But there's not a single warning reported either!
I think in older builds, warnings were being reported for such conversions, when /vo4+ was enabled, but not absolutely sure, so I am logging this for investigation/consideration.
*/

// /vo4+ enabled, warnings as errors enabled, warnings 165,219 are disabled
// Must report errors
#pragma options("vo4", on)      // signed - unsigned same size
#pragma options("vo11", on)     // conversions between different sizes and from fractional to integral
FUNCTION Start() AS VOID
LOCAL n AS INT
LOCAL d AS DWORD
LOCAL b AS BYTE
LOCAL w AS WORD
d := n
n := d
b := n
b := d
b := w
w := n
w := d

