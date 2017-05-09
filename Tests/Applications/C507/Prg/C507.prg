// 507. error XS9002: Parser: unexpected input 'cb'
/*
Problem is the "," in |x|, x[1]...
I guess it is just a typo in (Otto's) code, but VO does allow it.
Not sure we should allow it in x#, but in any case it would be nice to improve if possible
the parser message, because right now it is not indicative on what the actual problem is.

The original code had a long expression and spotting the "," was a real challenge :)
Vulcan does report a better error message, saying "unexpected token: ','", pointing to the comma char
*/
FUNCTION Start( ) AS VOID
LOCAL cb AS CODEBLOCK
cb := { |x|, x[1] == 1 }

