//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


#region defines
ENUM Concurrency
    MEMBER None				:= 0
    MEMBER Optimistic		:= 1
    MEMBER Stable			:= 2
    MEMBER Repeatable		:= 3
    MEMBER File				:= 4
    MEMBER User             := 1000
END ENUM
DEFINE ccOptimistic			:= Concurrency.Optimistic
DEFINE ccNone				:= Concurrency.None
DEFINE ccStable				:= Concurrency.Stable
DEFINE ccRepeatable		    := Concurrency.Repeatable
DEFINE ccFile				:= Concurrency.File
DEFINE ccUser				:= Concurrency.User
DEFINE DBC_INDEXNAME	:= 1
DEFINE DBC_INDEXPATH	:= 2
DEFINE DBC_ORDERS		:= 3
DEFINE DBC_TAGNAME		:= 1
DEFINE DBC_DUPLICATE		:= 2
DEFINE DBC_ASCENDING	:= 3
DEFINE DBC_KEYEXP		:= 4
DEFINE DBC_FOREXP		:= 5
DEFINE DBC_SYMBOL		:= 1
DEFINE DBC_NAME			:= 2
DEFINE DBC_FIELDSPEC		:= 3


/// <summary>
/// This enum describes the various notification messages that are used by VO compatible servers and clients
/// </summary>
ENUM Notify
    /// <include file="System.xml" path="doc/notifyfieldchange/*" />
    MEMBER FieldChange :=  -1
    /// <include file="System.xml" path="doc/notifyclose/*" />
    MEMBER Close :=  0
    /// <include file="System.xml" path="doc/notifycompletion/*" />
    MEMBER Completion :=  1
    /// <include file="System.xml" path="doc/notifycompletion/*" />
    MEMBER IntentToMove :=  2
    /// <include file="System.xml" path="doc/notifyrecordchange/*" />
    MEMBER RecordChange :=  3
    /// <include file="System.xml" path="doc/notifygobottom/*" />
    MEMBER GoBottom :=  4
    /// <include file="System.xml" path="doc/notifygotop/*" />
    MEMBER GoTop :=  5
    /// <include file="System.xml" path="doc/notifydelete/*" />
    MEMBER Delete :=  6
    /// <include file="System.xml" path="doc/notifyappend/*" />
    MEMBER Append :=  7
    /// <include file="System.xml" path="doc/notifyfilechange/*" />
    MEMBER FileChange :=  10
    /// <include file="System.xml" path="doc/notifyrelationchange/*" />
    MEMBER RelationChange :=  20
    /// <include file="System.xml" path="doc/notifyclearrelation/*" />
     MEMBER ClearRelation :=  25
    /// <include file="System.xml" path="doc/notifyconcurrencycontrolmode/*" />
    MEMBER ConcurrencyControlMode := 50
END ENUM

/// <include file="System.xml" path="doc/notifyfieldchange/*" />
DEFINE NOTIFYFIELDCHANGE        :=  Notify.FieldChange
/// <include file="System.xml" path="doc/notifyclearrelation/*" />
DEFINE NOTIFYCLEARRELATION      :=  Notify.ClearRelation
/// <include file="System.xml" path="doc/notifyclose/*" />
DEFINE NOTIFYCLOSE              :=  Notify.Close
/// <include file="System.xml" path="doc/notifycompletion/*" />
DEFINE NOTIFYCOMPLETION         :=  Notify.Completion
/// <include file="System.xml" path="doc/notifyintenttomove/*" />
DEFINE NOTIFYINTENTTOMOVE       :=  Notify.IntentToMove
 /// <include file="System.xml" path="doc/notifyrecordchange/*" />
DEFINE NOTIFYRECORDCHANGE       :=  Notify.RecordChange
 /// <include file="System.xml" path="doc/notifygobottom/*" />
DEFINE NOTIFYGOBOTTOM           :=  Notify.GoBottom
 /// <include file="System.xml" path="doc/notifygotop/*" />
DEFINE NOTIFYGOTOP              :=   Notify.GoTop
 /// <include file="System.xml" path="doc/notifydelete/*" />
DEFINE NOTIFYDELETE             :=   Notify.Delete
 /// <include file="System.xml" path="doc/notifyappend/*" />
DEFINE NOTIFYAPPEND             :=  Notify.Append
 /// <include file="System.xml" path="doc/notifyfilechange/*" />
DEFINE NOTIFYFILECHANGE         :=  Notify.FileChange
 /// <include file="System.xml" path="doc/notifyrelationchange/*" />
DEFINE NOTIFYRELATIONCHANGE     := Notify.RelationChange
 /// <include file="System.xml" path="doc/notifyconcurrencycontrolmode/*" />
DEFINE NOTIFYCONCURRENCYCONTROLMODE := Notify.ConcurrencyControlMode

DEFINE TYPE_MULTIMEDIA          := 42


DEFINE MAXFILENAME      := 260
DEFINE MAXEXTNAME       := 128
DEFINE MAXDIRNAME       := 256
DEFINE MAXDRIVENAME     := 128
#endregion
