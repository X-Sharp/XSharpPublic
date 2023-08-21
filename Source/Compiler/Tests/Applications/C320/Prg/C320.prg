// error XS0457: Ambiguous user defined conversions '__Psz.implicit operator __Psz(int)' and '__Psz.implicit operator __Psz(dword)' when converting from 'byte' to '__Psz'
// vulcan does not require any compiler options
FUNCTION Start() AS VOID
LOCAL DIM buf[10] AS BYTE
LOCAL c AS STRING
LOCAL u AS USUAL
buf[1] := 65
buf[2] := 66
buf[3] := 67
buf[4] := 0
? Psz2String( @buf[1] )
? Psz2String( @buf )
c := Psz2String( @buf )
? c
u := Psz2String( @buf[3] )
? u

LOCAL p AS PSZ
?
p := PSZ(_CAST, @buf[3])
? p
p := PSZ(_CAST, @buf)
? p

