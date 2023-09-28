function Start() as void strict
    try
        TEXT TO FILE test.txt
        some
        text in here
        !!!!
        ENDTEXT
        var cText := MemoRead("test.txt")
        ? cText
        xAssert(Alltrim(MemoLine(cText, 80, 2)) == "some")
        xAssert(Alltrim(MemoLine(cText, 80, 3)) == "text in here")
        xAssert(Alltrim(MemoLine(cText, 80, 4)) == "!!!!")
    catch e as exception
        ? e:Message
    end try

    _wait()


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
