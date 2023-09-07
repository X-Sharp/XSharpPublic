#pragma options("lb", on)
// https://github.com/X-Sharp/XSharpPublic/issues/963
#xtranslate new <Exp> ( [<args,...>] ) => <Exp>():new([<args>])

#translate ISARRAY(<v>)      =>  ( VALTYPE(<v>)=="A" )
#translate ISBLOCK(<v>)      =>  ( VALTYPE(<v>)=="B" )
#translate ISCHARACTER(<v>)  =>  ( VALTYPE(<v>)=="C" )
#translate ISDATE(<v>)       =>  ( VALTYPE(<v>)=="D" )
#translate ISLOGICAL(<v>)    =>  ( VALTYPE(<v>)=="L" )
#translate ISNIL(<v>)        =>  ( (<v>)==NIL )
#translate ISNUMBER(<v>)     =>  ( VALTYPE(<v>)=="N" )
#translate ISOBJECT(<v>)     =>  ( VALTYPE(<v>)=="O" )
#xtranslate \{<b> .. <e>\} => Range{<b>, <e>}



class Example
exported:
    Inline Method Init (arg)
    return
endclass

procedure main()
    testEpsilon()
    local o := new Example(2)
    xassert(ISOBJECT(o))
    o := NIL
    xassert(ISNIL(o))
    o := {1,2,3}
    xassert(ISARRAY(o))
    o := {||TRUE}
    xassert( ISBLOCK(o) )
    o := "abc"
    xassert( ISCHARACTER(o))
    o := 2022.01.01
    xassert( ISDATE(o) )
    o := TRUE
    xassert( ISLOGICAL(o))
    o := 10
    xassert( ISNUMBER(o) )
    local r := {1 .. 5}
    xassert( r != null)
    datetest()
return


CLASS Range
    PROPERTY x as long auto
    PROPERTY y as long auto
    CONSTRUCTOR(lx,ly)
        x := lx
        y := ly
END CLASS


#translate $<d>/<m>/<y> => ntod((<y>)*10000+(<m>)*100+(<d>))

function datetest  as void
local d := $ 18/02/2022
xassert(d == 2022.02.18)


function ntod(nNumber)
    return STOD(STR(nNumber,8,0))

#translate (<n1>.EQ.<n2>) =>    (IsZero((<n1>)-(<n2>)))



function testEpsilon
    local a,b,c
    a := 10.0001
    b := 20.0002
    c := 10.000
    xAssert((a.eq.b-c) == true)
    return nil

#define EPSILON 0.005
function isZero(n)
return (n <= EPSILON .and. n >= -EPSILON)




PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
