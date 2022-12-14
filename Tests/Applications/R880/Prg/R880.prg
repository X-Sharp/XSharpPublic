// https://github.com/X-Sharp/XSharpPublic/issues/1187
// https://github.com/X-Sharp/XSharpPublic/issues/1190
static class Test
    public static method GetVal() as logic
        local nAbc := 1 as int

        if (nAbc > 0)  // error here and not in the elseif line
            nop()
        elseif (self:nAbc == 0)
            nop()
        endif

        return true
end class


