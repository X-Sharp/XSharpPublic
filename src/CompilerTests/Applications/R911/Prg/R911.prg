function Start() as void strict
    try
        TEXT TO FILE C:\Output\test.txt
        some
        text in here
        !!!!
        ENDTEXT
        var cText := MemoRead("C:\Output\test.txt")
        ? cText
    catch e as exception
        ? e:Message
    end try

    _wait()
