Using System
Using System.Collections.Generic
Using System.Linq
Using System.Text

Function Start() As Void Strict
   LOCAL u AS USUAL
PRIVATE p
try
u := Today()
p := Today()

? GoMonth(u,1) // error XS0121
? GoMonth(p,1) // error XS0121
    ? Quarter(u, 1)
    ? Week(u,1)
    ? Week(u,2)
    ? Week(u,3)
    ? Dmy(u)
    ? Mdy(u)
    u := "abc"
    //? Mdy(u)
    //? Week(u,3)
    //? GoMonth(u,1) // error XS0121
    ? Quarter(u, 1)
catch e as exception
    ? e:ToString()
end try
    wait
