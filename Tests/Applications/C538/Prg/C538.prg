// 538. error XS0019: Operator '>=' cannot be applied to operands of type 'string' and 'string' 
FUNCTION Start( ) AS VOID
LOCAL a AS ARRAY
a := {65,67,66}
ASort(a,,, {|x,y| Chr(x)>=Chr(y)})
ASort(a,,, {|x,y| Chr(x)<=Chr(y)})
? a[1] , a[2] , a[3]
IF a[2] != 66
	THROW Exception{"Incorrect result"}
END IF
RETURN
