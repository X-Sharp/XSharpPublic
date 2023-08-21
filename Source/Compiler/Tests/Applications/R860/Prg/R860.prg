// https://github.com/X-Sharp/XSharpPublic/issues/1071
// /Namedarguments

function Start() as void strict
    var x := 1
    var testSym := #Test
    Log(testSym)
    Log(testSym = #Test2) // This line
    Log(x = 1) // this line

    console.ReadKey()
return

procedure Log(val as usual)
    var xx := val:ToString()
    ? xx
end procedure
