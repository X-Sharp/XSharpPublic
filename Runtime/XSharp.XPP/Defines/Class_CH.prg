//////////////////////////////////////////////////////////////////////
//
//  CLASS_CH.prg
//
//  
//  Contents:
//      constant for the dynamic creation of classes
//   
//////////////////////////////////////////////////////////////////////


// default
DEFINE CLASS_HIDDEN         := 0x0001   
DEFINE CLASS_PROTECTED      := 0x0002
DEFINE CLASS_EXPORTED       := 0x0003

DEFINE VAR_ASSIGN_HIDDEN    := 0x0010  
DEFINE VAR_ASSIGN_PROTECTED := 0x0020
// default is CLASS_... value
DEFINE VAR_ASSIGN_EXPORTED  := 0x0030   

// default
DEFINE VAR_INSTANCE         := 0x0100   
DEFINE VAR_CLASS            := 0x0300
DEFINE VAR_CLASS_SHARED     := 0x0B00

// default
DEFINE METHOD_INSTANCE      := 0x0010   
DEFINE METHOD_CLASS         := 0x0020

DEFINE METHOD_ACCESS        := 0x0400
DEFINE METHOD_ASSIGN        := 0x0800

// defines for :classDescribe(<nMode>)
DEFINE CLASS_DESCR_ALL          := 0
DEFINE CLASS_DESCR_CLASSNAME    := 1
DEFINE CLASS_DESCR_SUPERCLASSES := 2
DEFINE CLASS_DESCR_MEMBERS      := 3
DEFINE CLASS_DESCR_METHODS      := 4
DEFINE CLASS_DESCR_SUPERDETAILS := 5

// index into member array returned by :classDescribe()
DEFINE CLASS_MEMBER_NAME         := 1
DEFINE CLASS_MEMBER_ATTR         := 2
DEFINE CLASS_MEMBER_TYPE         := 3

// index into superclass array returned by :classDescribe()
DEFINE CLASS_SUPERCLASS_NAME     := 1
DEFINE CLASS_SUPERCLASS_ATTR     := 2
DEFINE CLASS_SUPERCLASS_TYPE     := 3

// index into method array returned by :classDescribe()
DEFINE CLASS_METHOD_NAME        := 1
DEFINE CLASS_METHOD_ATTR        := 2
DEFINE CLASS_METHOD_BLOCK       := 3
DEFINE CLASS_METHOD_VARNAME     := 4   // ACCESS/ASSIGN
DEFINE CLASS_METHOD_TYPES       := 5   // => Array



