// 638. error XS0103: The name 'Xs$Return' does not exist in the current context
FUNCTION Start( ) AS INT
	LOCAL c AS STRING
	LOCAL p AS PSZ
	c := "abc"
	p := String2Psz(c)
	? p
RETURN 0
