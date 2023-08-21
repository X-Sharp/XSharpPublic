function Start() as void strict
? Two
var xxx := OuterClass{}
xxx:TestMethod(OuterClassEventArgs{})

console.ReadKey()
return

class OuterClass

public property Dummy as int auto get set := 5

public method TestMethod(e as OuterClassEventArgs) as logic
return e:Dummy == 10

end class

class OuterClassEventArgs

public property Dummy as int auto get set := 10

end class
define two := Math.Sqrt(4)

