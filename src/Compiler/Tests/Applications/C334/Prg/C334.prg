// 334. error XS0103: The name 'CONTAINER_CLASS' does not exist in the current context

#define CONTAINER_CLASS "Ca_Container32"

FUNCTION Start() AS VOID
LOCAL s := CONTAINER_CLASS AS STRING
? s
? CONTAINER_CLASS // ok
LOCAL arr AS STRING[]
arr := <STRING>{CONTAINER_CLASS:ToUpper()}
? arr[1]
? CONTAINER_CLASS:ToUpper()

// original code
CLASS Vulcan.WinFormVOWindowHost //INHERIT Component
    PRIVATE classNeedTranslateTabToArrow := <STRING>{CONTAINER_CLASS:ToUpper()} AS STRING[]
END CLASS

