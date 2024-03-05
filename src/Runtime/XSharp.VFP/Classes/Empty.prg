//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Text
using System.Linq
using System.Diagnostics
using XSharp.Internal
/// <summary>
/// The FoxPro Empty class.
/// </summary>
[AllowLateBinding];
[DebuggerTypeProxy(TYPEOF(EmptyDebugView))];
class XSharp.VFP.Empty implements XSharp.IDynamicProperties, XSharp.IDynamicProperties2

    constructor()
        SELF:_InitProperties()
        return

    PROTECTED VIRTUAL METHOD _InitProperties AS VOID
        RETURN

    // include common vfp dynamic properties support
    // no other properties and fields needed
    #include "vfpproperties.xh"

    internal class EmptyDebugView
        private _value as Empty
        private property _count as int get _value:Properties:Count
        internal constructor (e as Empty)
            _value := e

        [DebuggerBrowsable(DebuggerBrowsableState.Collapsed)] ;
        [DebuggerDisplay("Count = {_count}", Type:="Dynamic Properties")];
        public property Properties as IList<NameValuePair> get _value:Properties:GetProperties()



    end class

end class

