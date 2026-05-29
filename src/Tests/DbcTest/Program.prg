USING System
USING System.IO

FUNCTION Start() AS VOID STRICT
    LOCAL cDataPath AS STRING
    cDataPath := Path.Combine(DataPath(), "DbcTest")
    IF !System.IO.Directory.Exists(cDataPath)
        System.IO.Directory.CreateDirectory(cDataPath)
    ENDIF
    TestRunner.RunAll(cDataPath)
    ? "Press any key..."
    Console.ReadKey()


    // Returns the path of the test data folder (same folder as the exe)
FUNCTION DataPath() AS STRING
    RETURN System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly():Location)
END FUNCTION

