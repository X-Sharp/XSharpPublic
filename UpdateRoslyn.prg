FUNCTION Start AS VOID                                
    VAR path := "C:\XSharp\Dev\Roslyn"
    VAR files := System.IO.Directory.GetFiles(path,"*.cs",System.IO.SearchOption.AllDirectories)
    LOCAL nCount := 0 AS LONG
    Prunefiles(files, REF nCount)    
    files := System.IO.Directory.GetFiles(path,"*.vb",System.IO.SearchOption.AllDirectories)
    Prunefiles(files, REF nCount)    
    
FUNCTION PruneFiles(files AS STRING[], nCount REF LONG) AS VOID    
    FOREACH VAR file IN files                                                                                     
        VAR text := System.IO.File.ReadAllText(file)
        IF text:Contains("XSHARP")
            ++nCount
            ? nCount, file
        ELSE
            System.IO.File.Delete(file)
        ENDIF
    NEXT   
    