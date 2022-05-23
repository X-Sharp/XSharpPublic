using System
using System.Collections.Generic
using System.Linq
using System.Text
#pragma options("lb", on)
class Test
	public property Values as List<int> auto get set := List<int>{}
end class

function Start() as void strict
	local obj := Test{} as object
	obj:Values:Add(1)
	obj:Values:Add(2)

	foreach var i in obj:Values
		Console.WriteLine(((int)i):ToString())
	next
	Console.ReadLine()
	return


