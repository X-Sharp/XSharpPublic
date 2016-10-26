// 247. error XS9002: Parser: unexpected input '->'
CLASS Testclass
EXPORT wWorkArea AS DWORD
METHOD Test() AS VOID
(SELF:wWorkArea)->(VODBCloseArea( ))
END CLASS

