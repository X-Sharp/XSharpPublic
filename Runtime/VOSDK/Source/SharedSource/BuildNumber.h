// BuildNumber.h
#ifndef BUILDNUMBER_H
#define BUILDNUMBER_H
#define PRODUCT_NAME     "XSharp"

#define VERSION_NUMBER         2,0,0,9
#define VERSION_NUMBER_STR     "2.0.0.9"
// This is the file version number, which is ignored by .NET but used by Windows installer to determine
// whether one file is newer than another.
// This typically would change if we're generating a patch, otherwise it should be the same as VERSION_NUMBER
#define FILEVERSION_NUMBER         2,0,0,9
#define FILEVERSION_NUMBER_STR     "2.0.0.9"
// This is the file version number, which is ignored by .NET but used by Windows installer to determine
// whether one file is newer than another.
// This typically would change if we're generating a patch, otherwise it should be the same as VERSION_NUMBER

#define COMPANY_NAME "XSharp B.V."
#define COPYRIGHT_STR "Copyright © 1993-2018 Computer Associates & XSharp BV, All rights reserved"
#ifdef __DEBUG__
	#define ASSEMBLY_CONFIGURATION "Debug"
#else
	#define ASSEMBLY_CONFIGURATION "Release"
#endif
#endif  // BUILDNUMBER_H 

