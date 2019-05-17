/// <summary>Creates an object from a class definition or an Automation-enabled application.</summary>
/// <param name="cClassName">Specifies the class or OLE object from which the new object is created.</param>
/// <param name="_args">These optional parameters are used to pass values to the Init event procedure for the class.
/// The Init event is executed when you issue CREATEOBJECT( ) and allows you to initialize the object.</param>
/// <returns>The object that was created</returns>
/// <seealso cref='M:XSharp.RT.Functions.CreateInstance(XSharp.__Usual)' >CreateInstance</seealso>

FUNCTION CreateObject(cClassName, _args ) AS OBJECT CLIPPER
    LOCAL args AS USUAL[]
    args := USUAL[]{PCount()}
    FOR nArg := 1 TO PCount()
        args[nArg] := _GetFParam(nArg)
    NEXT
    RETURN CreateInstance(args)
