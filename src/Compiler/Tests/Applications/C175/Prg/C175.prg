// 175. error XS1503: Argument 1: cannot convert from 'uint' to 'string'
FUNCTION Start() AS VOID
LOCAL nAlias AS DWORD
nAlias := Select("test.dbf")
(nAlias)->DBSkip()

