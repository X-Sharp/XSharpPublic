FUNCTION Start( ) AS VOID
    try
        ? DefaultExt("abc","def")
        ? DefaultExt("abc.","def")
        ? DriveType("C:")
        ? DriveType("G:")
        ? DriveType("X:")
        ? DriveType("A:")
        ? FullPath("testss.txt", "xsharp.core.dll")
        for var i := 1 to 34
            ? "Sysmetric", i, SysMetric(i)
        next
    catch e as exception
        ? e:ToString()
    end try
    wait
    RETURN

