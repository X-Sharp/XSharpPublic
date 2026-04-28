//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

begin namespace XSharp
/// <include file="XSharp.RT.Docs.xml" path="doc/IIndexedProperties/*" />
interface IIndexedProperties
    /// <include file="XSharp.RT.Docs.xml" path="doc/IIndexedProperties.self_index/*" />
    property self[index as int   ] as usual get set
    /// <include file="XSharp.RT.Docs.xml" path="doc/IIndexedProperties.self_name/*" />
    property self[name  as string] as usual get set
end interface

/// <include file="XSharp.RT.Docs.xml" path="doc/IIndexer/*" />
interface IIndexer
    /// <include file="XSharp.RT.Docs.xml" path="doc/IIndexer.self_index/*" />
    public property self[index params int[]] as usual get set
    /// <include file="XSharp.RT.Docs.xml" path="doc/IIndexer.self_index/*" />
    public property self[index as int] as usual get set
    /// <include file="XSharp.RT.Docs.xml" path="doc/IIndexer.self_index1/*" />
    public property self[index1 as int, index2 as int] as usual get set
end interface

/// <include file="XSharp.RT.Docs.xml" path="doc/INamedIndexer/*" />
interface INamedIndexer
    /// <include file="XSharp.RT.Docs.xml" path="doc/INamedIndexer.self_index/*" />
    public property self[index as int, name as string] as usual get set
end interface


/// <include file="XSharp.RT.Docs.xml" path="doc/IDynamicProperties/*" />
interface IDynamicProperties
    /// <include file="XSharp.RT.Docs.xml" path="doc/IDynamicProperties.GetPropertyNames/*" />
    public method GetPropertyNames() as string[]
    /// <include file="XSharp.RT.Docs.xml" path="doc/IDynamicProperties.NoIvarGet/*" />
    public method NoIvarGet(cName as string) as usual
    /// <include file="XSharp.RT.Docs.xml" path="doc/IDynamicProperties.NoIvarPut/*" />
    public method NoIvarPut(cName as string, uValue as usual) as void
end interface

interface IDynamicProperties2
    public method _RemoveProperty(cName as string) as logic
    public method _AddProperty(cPropertyName as string, uValue as usual, nVisibility as long, cDescription as string) as logic

end interface

/// <include file="XSharp.RT.Docs.xml" path="doc/IMacroCompilerUsual/*" />
interface IMacroCompilerUsual
    /// <include file="XSharp.RT.Docs.xml" path="doc/IMacroCompilerUsual.CompileCodeblock/*" />
    public method CompileCodeblock(macro as string , lAllowSingleQuotes as logic, module as System.Reflection.Module) as XSharp._Codeblock

    /// <include file="XSharp.RT.Docs.xml" path="doc/IMacroCompilerUsual.CompileCodeblock/*" />
    public method CompileCodeblock(macro as string ) as XSharp._Codeblock

end interface

/// <include file="XSharp.RT.Docs.xml" path="doc/IRtCodeblock/*" />
interface IRtCodeblock inherit ICodeblock2
    /// <include file="XSharp.RT.Docs.xml" path="doc/IRtCodeblock.IsBlock/*" />
    property IsBlock as logic get
    /// <include file="XSharp.RT.Docs.xml" path="doc/IRtCodeblock.Eval/*" />
    method Eval(args params usual[]) as usual
end interface

/// <include file="XSharp.RT.Docs.xml" path="doc/ILateBound/*" />
interface ILateBound
    /// <include file="XSharp.RT.Docs.xml" path="doc/ILateBound.NoIvarGet/*" />
    method NoIvarGet(cName as string) as usual
    /// <include file="XSharp.RT.Docs.xml" path="doc/ILateBound.NoIvarGet/*" />
    method NoIvarGetSelf(cName as string) as usual
    /// <include file="XSharp.RT.Docs.xml" path="doc/ILateBound.NoIvarPut/*" />
    method NoIvarPut(cName as string, uValue as usual) as void
    /// <include file="XSharp.RT.Docs.xml" path="doc/ILateBound.NoIvarPut/*" />
    method NoIvarPutSelf(cName as string, uValue as usual) as void
    /// <include file="XSharp.RT.Docs.xml" path="doc/ILateBound.NoMethod/*" />
    method NoMethod() as usual clipper
END INTERFACE
interface IWrappedObject
    property Object as object get
    property Type   as System.Type get
end interface

end namespace
