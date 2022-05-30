function start as void
    try
    local aStrings as ARRAY OF STRING
    aStrings := {}
    AAdd(aStrings, 123)
    ? aStrings[1]
    catch e as exception
        ? e:ToString()
    end try
    wait
    RETURN
