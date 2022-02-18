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
    ? o                
    ? ISOBJECT(o)
    o := NIL
    ? ISNIL(o)
    o := {1,2,3}
    ? ISARRAY(o)
    o := {||TRUE}
    ? ISBLOCK(o) 
    o := "abc"
    ? ISCHARACTER(o)
    o := 2022.01.01
    ? ISDATE(o) 
    o := TRUE
    ? ISLOGICAL(o)
    o := 10
    ? ISNUMBER(o)
    local r := {1 .. 5}     
    ? r
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
? d          


function ntod(nNumber)
    return nNumber

#translate (<n1>.EQ.<n2>) =>    (IsZero((<n1>)-(<n2>)))



function testEpsilon
    local a,b,c
    a := 10.0001
    b := 20.0002
    c := 10.000
    ? (a.eq.b-c)
    return nil
    
#define EPSILON 0.005
function isZero(n)
return (n <= EPSILON .and. n >= -EPSILON)

    
