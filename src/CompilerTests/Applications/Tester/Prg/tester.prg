// Core dialect

FUNCTION start() as void
var (x , y ) := tuple{42,"robert"}
local a as (int, string)
local (b as int, c as string) := tuple(42,"robert")
a := (x,y)
? x, y
? a
? b,c
RETURN
