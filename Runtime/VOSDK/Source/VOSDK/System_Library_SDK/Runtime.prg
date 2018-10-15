DEFINE WAGNER_STRING      := (0)
DEFINE WAGNER_OBJECT      := (2)
DEFINE WAGNER_ARRAY       := (4)
DEFINE WAGNER_FIXED       := (6)
DEFINE WAGNER_FLOAT       := (8)
DEFINE WAGNER_ARRAY_PAGE  := (10)  // no root dynamic type!
DEFINE WAGNER_BINARY      := (0x40)	// Not used
DEFINE WAGNER_STATIC      := (0x80)	// Entry in Static memory
//
//  Defines for _VO_COLLECTINFO.wCollectCount:
//
DEFINE WAGNER_FORWARD      := (0xFFFD)        // -3
//
//  Defines for _VO_COLLECTINFO.bFlag:
//
DEFINE WAGNER_HAS_AXIT     	:= (0x80)
DEFINE WAGNER_AXIT_CALLED  	:= (0x04)
DEFINE WAGNER_OLDSPACE_STATE 	:= (0x02)
//
// Runtime Class Information Structures
//
DEFINE MAX_INST	:= 10
// ProcName Stack structures
