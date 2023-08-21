
FUNCTION Start( ) AS VOID
	local cb as CodeBlock 
	cb := {|n| n+1}   
	xAssert(eval(cb,1)=2)
	cb := {|n as int| n+1}   
	xAssert(eval(cb,1)=2)
	cb := {|n as dword| n+1}   
	xAssert(eval(cb,1)=2)   
	
	cb := {|n | n++,n++,n++,n}   
	xAssert(eval(cb,1)=4)   

	cb := {|n as int| n++,n++,n++,n}   
	xAssert(eval(cb,1)=4)   

	cb := {|n | 
		n++
		n++
		n++
		n++
		return n
		}   
	xAssert(eval(cb,1)=5)   

	cb := {|n as int | 
		n++
		n++
		n++
		n++
		return n
		}   
	xAssert(eval(cb,1)=5)   

	
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
