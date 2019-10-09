//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
using XSharp.RDD.Enums

/// <summary>Register an object that will receive notification messages for workarea events.</summary>
/// <returns>TRUE when registration succceeded. FALSE when the client was already registered</returns>
/// <seealso cref="M:XSharp.Core.Functions.DbUnRegisterClient(XSharp.IDbNotify)"/>
/// <seealso cref="T:XSharp.IDbNotify"/>
/// <seealso cref="T:XSharp.DbNotifyEventHandler"/>
FUNCTION DbRegisterClient(oClient as IDbNotify) AS LOGIC
    CoreDb.Notify += oClient:Notify
    RETURN TRUE

/// <summary>Unregister an object that receives notification messages for workarea events.</summary>
/// <returns>TRUE when unregistration succceeded. FALSE when the the client was not registered.</returns>
/// <seealso cref="M:XSharp.Core.Functions.DbRegisterClient(XSharp.IDbNotify)"/>
/// <seealso cref="T:XSharp.IDbNotify"/>
/// <seealso cref="T:XSharp.DbNotifyEventHandler"/>
FUNCTION DbUnRegisterClient(oClient as IDbNotify) AS LOGIC
    CoreDb.Notify -= oClient:Notify
    RETURN TRUE


/// <summary>Provides data for the Notify event of the IDbNotify interface.</summary>
/// <seealso cref="T:XSharp.IDbNotify"/>
/// <seealso cref="T:XSharp.DbNotifyEventHandler"/>
CLASS XSharp.DbNotifyEventArgs
    /// <summary>The type of event that happened.</summary>
    PUBLIC PROPERTY Type    as DbNotificationType AUTO GET PRIVATE SET
    /// <summary>Specific data that describes the object.</summary>
    PUBLIC PROPERTY Data    as OBJECT AUTO GET PRIVATE SET
    /// <exclude />
    INTERNAL CONSTRUCTOR (nType as DbNotificationType, oData as OBJECT)
        SELF:Type   := nType
        SELF:Data   := oData
        RETURN

END CLASS

/// <summary>The delegate that defines the Workarea Eventhandler</summary>
/// <param name="sender">Workarea for which the event happened.
/// <note>The <paramref name="sender">sender</paramref> object may be NULL for operations that work on all workareas, such as DbCloseAll() </note></param>
/// <param name="e">Object that describes the event that happened.</param>
/// <seealso cref="T:XSharp.RDD.IRDD"/>
/// <seealso cref="T:XSharp.DbNotifyEventArgs"/>
// note this method matches the signature of XSharp.IDbNotify.Notify()
PUBLIC DELEGATE XSharp.DbNotifyEventHandler(osender as XSharp.RDD.IRDD, e as XSharp.DBNotifyEventArgs) AS VOID


/// <summary>
/// This interface must be implemented by objects that register themselves for DB Notifications
/// </summary>
/// <seealso cref="M:XSharp.Core.Functions.DbRegisterClient(XSharp.IDbNotify)"/>
/// <seealso cref="M:XSharp.Core.Functions.DbUnRegisterClient(XSharp.IDbNotify)"/>
/// <seealso cref="T:XSharp.RDD.IRDD"/>
/// <seealso cref="T:XSharp.DbNotifyEventArgs"/>
INTERFACE XSharp.IDbNotify
    /// <summary>This message is sent to clients when a workarea event happens</summary>
    /// <param name="sender">Workarea for which the event happened.
    /// <note>The <paramref name="sender">sender</paramref> object may be NULL for operations that work on all workareas, such as DbCloseAll() </note></param>
    /// <param name="e">Object that describes the event that happened.</param>
    // note this method matches the signature of XSharp.DbNotifyEventHandler
    METHOD Notify(sender AS XSharp.RDD.IRDD, e as DbNotifyEventArgs) AS VOID
END INTERFACE 
