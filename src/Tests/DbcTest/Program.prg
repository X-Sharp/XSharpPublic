Using System
Using System.Collections.Generic
Using System.Linq
Using System.Text

Function Start() As Void Strict
    System.Console.OutputEncoding = System.Text.Encoding.GetEncoding(936)
    Use  c:\temp\testchar
    DoWork()
    Use  c:\temp\testmemo
    DoWork()
    Use  c:\temp\testvarchar
    DoWork()
    wait
    Return
End Function
FUNCTION DoWork()
    FIELD a, b
    Append Blank
    Replace a With "005", b With "中国"
    ? a
    ? b
    RETURN TRUE
