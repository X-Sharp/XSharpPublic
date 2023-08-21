FUNCTION Start() AS VOID
LOCAL pt AS PTR
LOCAL pz AS PSZ

pz := String2Psz("This is a PSZ string")
pt := pz
pz := String2Psz("not this!")

pz := (PSZ) pt // error
pz := pt // this works!

? pz

PszFunc(pt) // this works
PszFunc(PSZ(_CAST,pt)) // this works
PszFunc((PSZ) pt)

FUNCTION PszFunc(p AS PSZ) AS VOID
? p

