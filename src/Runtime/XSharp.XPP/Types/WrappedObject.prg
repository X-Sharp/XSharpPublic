
using System
using System.Collections.Generic
using System.Text

begin namespace XSharp.XPP

/// <summary>
/// The XppWrappedObject class.
/// </summary>
class __XppWrappedObject implements IWrappedObject
    property Object as object auto get private set
    property Type   as System.Type auto get private set

    constructor(oObject as object, type as System.Type)
        self:Object := oObject
        self:Type   := type
        return

end class
end namespace // XSharp.XPP.Types
