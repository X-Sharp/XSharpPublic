#pragma options("vo7", on) // allow @ for REF
#xtranslate __xlangext_SuppressNoInit(<h1> [, <hn>]) => (<h1> := NIL[, <hn> := NIL ])
#xtranslate __xlangext_Apply(<f>, <h1> [, <hn>]) => <f><h1>[, <f><hn>]
#xtranslate __xlangext_Last(<h>) => <h>
#xtranslate __xlangext_Last(<h>, <t,...>) => __xlangext_Last(<t>)
//
#xtranslate LOCAL {<args,...>} := <expr>;
    =>;
    LOCAL <args> := ({<args>} := <expr>, __xlangext_Last(<args>))

#xtranslate {<args,...>} := <expr>;
    =>;
    (__xlangext_SuppressNoInit(<args>),;
    xlangext_AssignTuple(<expr>, __xlangext_Apply(@, <args>)))


FUNCTION Start() AS VOID
LOCAL {a, b, c} := _GetTuple()
? a,b,c
xAssert(a == 1)
xAssert(b == 2)
xAssert(c == 3)
{a, b, c} := _GetTuple()
? a,b,c
xAssert(a == 1)
xAssert(b == 2)
xAssert(c == 3)
LOCAL {d, e} := _GetTuple()
? d,e
xAssert(d == 1)
xAssert(e == 2)
LOCAL {f, g,h,i} := _GetTuple()
? f, g,h,i
xAssert(f == 1)
xAssert(g == 2)
xAssert(h == 3)
xAssert(i == NIL)


function xlangext_AssignTuple(src) as usual
    local i   as int
    local len as int
    // src contains the array
    // the other parameters are passed by reference in the Xs$Args array from
    // position 2 on
    len := min(len(src), pCount() - 1)
    for i := 1 to len
        _ARGS()[i+1] := src[i]
    next
return NIL

function _GetTuple() AS USUAL
    return {1,2,3}


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
