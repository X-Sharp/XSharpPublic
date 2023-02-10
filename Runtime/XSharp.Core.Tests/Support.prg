FUNCTION TempFile(cExt AS STRING) AS STRING
    VAR result := Guid.NewGuid():ToString()
    RETURN System.IO.Path.ChangeExtension(result, cExt)
