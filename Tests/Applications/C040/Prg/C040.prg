// 40. error XS1057: 'StaticClass.sp': static classes cannot contain protected members
#pragma warnings(1057, off) // static classes cannot have protected members
// vulcan allows it
STATIC CLASS StaticClass
	STATIC PROTECT sp AS INT
END CLASS

