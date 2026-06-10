// Test for    https://github.com/X-Sharp/XSharpPublic/issues/1966


function Start as void
local x as string
x := time()
if x:StartsWith('1')
    var user := UserId()
endif
return

function UserId() as string Clipper
    return "MrData"

