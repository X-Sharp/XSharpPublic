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

ENUM Notify
    MEMBER FieldChange :=  -1
    MEMBER Close :=  0
    MEMBER Completion :=  1
    MEMBER IntentToMove :=  2
    MEMBER RecordChange :=  3
    MEMBER GoBottom :=  4
    MEMBER GoTop :=  5
    MEMBER Delete :=  6
    MEMBER Append :=  7
    MEMBER FileChange :=  10
    MEMBER RelationChange :=  20
    MEMBER ClearRelation :=  25
    MEMBER ConcurrencyControlMode := 50
END ENUM

DEFINE NOTIFYFIELDCHANGE        :=  Notify.FieldChange
DEFINE NOTIFYCLEARRELATION      :=  Notify.ClearRelation
DEFINE NOTIFYCLOSE              :=  Notify.Close
DEFINE NOTIFYCOMPLETION         :=  Notify.Completion
DEFINE NOTIFYINTENTTOMOVE       :=  Notify.IntentToMove
DEFINE NOTIFYRECORDCHANGE       :=  Notify.RecordChange
DEFINE NOTIFYGOBOTTOM           :=  Notify.GoBottom
DEFINE NOTIFYGOTOP              :=   Notify.GoTop
DEFINE NOTIFYDELETE             :=   Notify.Delete
DEFINE NOTIFYAPPEND             :=  Notify.Append
DEFINE NOTIFYFILECHANGE         :=  Notify.FileChange
DEFINE NOTIFYRELATIONCHANGE     := Notify.RelationChange
DEFINE NOTIFYCONCURRENCYCONTROLMODE := Notify.ConcurrencyControlMode
DEFINE TYPE_MULTIMEDIA          := 42

DEFINE MAXFILENAME      := 260
DEFINE MAXEXTNAME       := 128
DEFINE MAXDIRNAME       := 256
DEFINE MAXDRIVENAME     := 128
#endregion
