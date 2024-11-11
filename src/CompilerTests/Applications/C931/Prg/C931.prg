// 931. Problem with the <> operator

#pragma options (lb,on)
FUNCTION Start() AS VOID
	LOCAL n := 1 AS INT
	? n<>-123
	IF n<>+456
		NOP
	END IF

	LOCAL u AS USUAL
	u := System.Collections.ArrayList{}
	? u:Count <> +1
	? u:Count <> -1
	? u:Count<>+1
	? u:Count<>-1
RETURN
