//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING XSharp.RDD
/// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType/*" />
ENUM XSharp.RDD.DatabasePropertyType
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Null/*" />
    MEMBER Null             := 0
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Path/*" />
    MEMBER Path             := 1
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Class/*" />
    MEMBER Class            := 2
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_3/*" />
    MEMBER Unknown_3         := 3
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_4/*" />
    MEMBER Unknown_4         := 4
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_5/*" />
    MEMBER Unknown_5         := 5
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_6/*" />
    MEMBER Unknown_6         := 6
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Comment/*" />
    MEMBER Comment          := 7
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_8/*" />
    MEMBER Unknown_8         := 8
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.RuleExpression/*" />
    MEMBER RuleExpression   := 9
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.RuleText/*" />
    MEMBER RuleText         := 10
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DefaultValue/*" />
    MEMBER DefaultValue     := 11
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.ParameterList/*" />
    MEMBER ParameterList    := 12
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.RelatedChild/*" />
    MEMBER RelatedChild     := 13
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.InsertTrigger/*" />
    MEMBER InsertTrigger    := 14
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.UpdateTrigger/*" />
    MEMBER UpdateTrigger    := 15
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DeleteTrigger/*" />
    MEMBER DeleteTrigger    := 16
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.IsUnique/*" />
    MEMBER IsUnique         := 17
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.RelatedTable/*" />
    MEMBER RelatedTable     := 18
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.RelatedTag/*" />
    MEMBER RelatedTag       := 19
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.PrimaryKey/*" />
    MEMBER PrimaryKey       := 20
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_21/*" />
    MEMBER Unknown_21        := 21
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_22/*" />
    MEMBER Unknown_22        := 22
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_23/*" />
    MEMBER Unknown_23        := 23
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Version/*" />
    MEMBER Version          := 24
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_25/*" />
    MEMBER Unknown_25        := 25
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_26/*" />
    MEMBER Unknown_26        := 26
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_27/*" />
    MEMBER Unknown_27        := 27
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.BatchUpdateCount/*" />
    MEMBER BatchUpdateCount := 28
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DataSource/*" />
    MEMBER DataSource       := 29
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_30/*" />
    MEMBER Unknown_30        := 30
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_31/*" />
    MEMBER Unknown_31        := 31
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.ConnectName/*" />
    MEMBER ConnectName      := 32
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_33/*" />
    MEMBER Unknown_33        := 33
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_34/*" />
    MEMBER Unknown_34        := 34
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.UpdateName/*" />
    MEMBER UpdateName       := 35
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.FetchMemo/*" />
    MEMBER FetchMemo        := 36
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.FetchSize/*" />
    MEMBER FetchSize        := 37
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.KeyField/*" />
    MEMBER KeyField         := 38
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.MaxRecords/*" />
    MEMBER MaxRecords       := 39
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.ShareConnection/*" />
    MEMBER ShareConnection  := 40
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.SourceType/*" />
    MEMBER SourceType       := 41
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.SQL/*" />
    MEMBER SQL              := 42
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Tables/*" />
    MEMBER Tables           := 43
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.SendUpdates/*" />
    MEMBER SendUpdates      := 44
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Updatable/*" />
    MEMBER Updatable        := 45
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.UpdateType/*" />
    MEMBER UpdateType       := 46
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.UseMemoSize/*" />
    MEMBER UseMemoSize      := 47
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.WhereType/*" />
    MEMBER WhereType        := 48
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_49/*" />
    MEMBER Unknown_49        := 49
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DisplayClass/*" />
    MEMBER DisplayClass     := 50
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DisplayClassLibrary/*" />
    MEMBER DisplayClassLibrary := 51
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_52/*" />
    MEMBER Unknown_52        := 52
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_53/*" />
    MEMBER Unknown_53        := 53
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.InputMask/*" />
    MEMBER InputMask        := 54
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Format/*" />
    MEMBER Format           := 55
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Caption/*" />
    MEMBER Caption          := 56
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_57/*" />
    MEMBER Unknown_57        := 57
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_58/*" />
    MEMBER Unknown_58        := 58
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_59/*" />
    MEMBER Unknown_59        := 59
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_60/*" />
    MEMBER Unknown_60        := 60
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_61/*" />
    MEMBER Unknown_61        := 61
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_62/*" />
    MEMBER Unknown_62        := 62
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_63/*" />
    MEMBER Unknown_63        := 63
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Asynchronous/*" />
    MEMBER Asynchronous     := 64
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.BatchMode/*" />
    MEMBER BatchMode        := 65
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.ConnectString/*" />
    MEMBER ConnectString    := 66
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.ConnectTimeout/*" />
    MEMBER ConnectTimeout   := 67
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DispLogin/*" />
    MEMBER DispLogin        := 68
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DispWarnings/*" />
    MEMBER DispWarnings     := 69
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.IdleTimeout/*" />
    MEMBER IdleTimeout      := 70
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.QueryTimeOut/*" />
    MEMBER QueryTimeOut     := 71
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Password/*" />
    MEMBER Password         := 72
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Transactions/*" />
    MEMBER Transactions     := 73
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.UserId/*" />
    MEMBER UserId           := 74
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.WaitTime/*" />
    MEMBER WaitTime         := 75
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.TimeStamp/*" />
    MEMBER TimeStamp        := 76
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DataType/*" />
    MEMBER DataType         := 77
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.PacketSize/*" />
    MEMBER PacketSize       := 78
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Database/*" />
    MEMBER Database         := 79
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Prepared/*" />
    MEMBER Prepared         := 80
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.CompareMemo/*" />
    MEMBER CompareMemo      := 81
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.FetchAsNeeded/*" />
    MEMBER FetchAsNeeded    := 82
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.Unknown_83/*" />
    MEMBER Unknown_83        := 83
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.OfflineRecs/*" />
    MEMBER OfflineRecs       := 84
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.OfflineRemRecs/*" />
    MEMBER OfflineRemRecs   := 85
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DBCEventFileName/*" />
    MEMBER DBCEventFileName := 86
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DBCEvents/*" />
    MEMBER DBCEvents        := 87
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.AllowSimultaneousFetch/*" />
    MEMBER AllowSimultaneousFetch := 88
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.DisconnectRollback/*" />
    MEMBER DisconnectRollback := 89
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.OffLine/*" />
    MEMBER OffLine          := 99
    /// <include file="XSharp.Core.Docs.xml" path="doc/DatabasePropertyType.ColumnName/*" />
    MEMBER ColumnName   := 100
END ENUM

