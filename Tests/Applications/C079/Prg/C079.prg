// 79. error XS0119: 'Functions.Directory()' is a method, which is not valid in the given context
#using System.IO
FUNCTION Directory() AS INT
Directory.CreateDirectory("")
RETURN 0

