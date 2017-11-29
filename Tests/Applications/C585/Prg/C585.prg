FUNCTION Start AS VOID
    LOCAL n, nFrom, nTo, nStep, nForward, nBackward  AS INT

    nFrom := 1

    nTo      := 5

    nStep   := 1

    nForward := 0

    FOR n := nFrom TO nTo STEP nStep

      ++ nForward

    NEXT

    nFrom := 5

    nTo      := 1

    nStep   := -1
                         
    nBackward := 0

    FOR n := nFrom TO nTo STEP nStep

      ++ nBackward

    NEXT
    ? "Forward", nForward
    ? "Backward", nBackward
    RETURN
