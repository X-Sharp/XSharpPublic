//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.Enums

/// <include file="XSharp.Core.Docs.xml" path="doc/DbRegisterClient/*" />
FUNCTION DbRegisterClient(oClient AS IDbNotify) AS LOGIC
    CoreDb.Notify += oClient:Notify
    RETURN TRUE

/// <include file="XSharp.Core.Docs.xml" path="doc/DbUnRegisterClient/*" />
FUNCTION DbUnRegisterClient(oClient AS IDbNotify) AS LOGIC
    CoreDb.Notify -= oClient:Notify
    RETURN TRUE


/// <include file="XSharp.Core.Docs.xml" path="doc/DbNotifyEventArgs/*" />
CLASS XSharp.DbNotifyEventArgs
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbNotifyEventArgs.Type/*" />
    PUBLIC PROPERTY Type    AS DbNotificationType AUTO GET PRIVATE SET
    /// <include file="XSharp.Core.Docs.xml" path="doc/DbNotifyEventArgs.Data/*" />
    PUBLIC PROPERTY Data    AS OBJECT AUTO GET PRIVATE SET
    /// <exclude />
    INTERNAL CONSTRUCTOR (nType AS DbNotificationType, oData AS OBJECT)
        SELF:Type   := nType
        SELF:Data   := oData
        RETURN

END CLASS

/// <summary>The delegate that defines the Workarea Eventhandler</summary>
/// <param name="sender">Workarea for which the event happened.
/// <note>The <paramref name="sender">sender</paramref> object may be NULL for operations that work on all Workareas, such as DbCloseAll() </note></param>
/// <param name="e">Object that describes the event that happened.</param>
/// <seealso cref="T:XSharp.RDD.IRdd"/>
/// <seealso cref="DbNotifyEventArgs"/>
// note this method matches the signature of XSharp.IDbNotify.Notify()
PUBLIC DELEGATE XSharp.DbNotifyEventHandler(osender AS XSharp.RDD.IRdd, e AS XSharp.DbNotifyEventArgs) AS VOID


/// <include file="XSharp.Core.Docs.xml" path="doc/IDbNotify/*" />
INTERFACE XSharp.IDbNotify
    /// <summary>This message is sent to clients when a Workarea event happens</summary>
    /// <param name="sender">Workarea for which the event happened.
    /// <note>The <paramref name="sender">sender</paramref> object may be NULL for operations that work on all Workareas, such as DbCloseAll() </note></param>
    /// <param name="e">Object that describes the event that happened.</param>
    // note this method matches the signature of XSharp.DbNotifyEventHandler
    METHOD Notify(sender AS XSharp.RDD.IRdd, e AS DbNotifyEventArgs) AS VOID
END INTERFACE 
