//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <exclude />
STATIC CLASS XSharp.VFP.DBProperty
    // Connection properties
    CONST Asynchronous              := "asynchronous" AS STRING
    CONST BatchMode                 := "batchmode" AS STRING
    CONST Comment                   := "comment" AS STRING
    CONST ConnectString             := "connectstring" AS STRING
    CONST ConnectTimeout            := "connecttimeout" AS STRING
    CONST Database                  := "database" AS STRING
    CONST DataSource                := "datasource" AS STRING
    CONST DisconnectRollback        := "disconnectrollback" AS STRING
    CONST DispLogin                 := "displogin" AS STRING
    CONST DispWarnings              := "dispwarnings" AS STRING
    CONST IdleTimeout               := "idletimeout" AS STRING
    CONST PacketSize                := "packetsize" AS STRING
    CONST PassWord                  := "password" AS STRING
    CONST QueryTimeout              := "querytimeout" AS STRING
    CONST Transactions              := "transactions" AS STRING
    CONST UserId                    := "userid" AS STRING
    CONST WaitTime                  := "waittime" AS STRING

    // Database properties
    //CONST Comment                   := "comment" AS STRING
    CONST DBCEventFileName          := "dbceventfilename" AS STRING
    CONST DBCEvents                 := "dbcevents" AS STRING
    CONST Version                   := "version" AS STRING

    // Field properties for tables
    CONST Caption                   := "caption" AS STRING
    //CONST Comment                   := "comment" as string
    CONST DefaultValue              := "defaultvalue" AS STRING
    CONST DisplayClass              := "displayclass" AS STRING
    CONST DisplayClassLibrary       := "displayclasslibrary" AS STRING
    CONST Format                    := "format" AS STRING
    CONST InputMask                 := "inputmask" AS STRING
    CONST RuleExpression            := "ruleexpression" AS STRING
    CONST RuleText                  := "ruletext" AS STRING

    // Field properties for views
    //CONST Caption                   := "caption" as string
    //CONST Comment                   := "comment" as string
    CONST DataType                  := "datatype" AS STRING
    //CONST DisplayClass              := "displayclass" as string
    //CONST DisplayClassLibrary       := "displayclasslibrary" as string
    //CONST DefaultValue              := "defaultvalue" as string
    CONST KeyField                  := "keyfield" AS STRING
    //CONST RuleExpression            := "ruleexpression" as string
    //CONST RuleText                  := "ruletext" as string
    CONST Updatable                 := "updatable" AS STRING
    CONST UpdateName                := "updatename" AS STRING
    
    // Table properties
    CONST DeleteTrigger     := "deletetrigger" AS STRING
    CONST InsertTrigger     := "inserttrigger" AS STRING
    CONST Path              := "path" AS STRING
    CONST PrimaryKey        := "primarykey" AS STRING
    //CONST RuleExpression    := "ruleexpression" AS STRING
    //CONST RuleText          := "ruletext" AS STRING
    CONST UpdateTrigger     := "updatetrigger" AS STRING

    // View properties
    CONST AllowSimultaneousFetch    := "allowsimultaneousfetch" AS STRING
    CONST BatchUpdateCount          := "batchupdatecount" AS STRING
    //CONST Comment                   := "comment" as string
    CONST CompareMemo               := "comparememo" AS STRING
    CONST ConnectName               := "connectname" AS STRING
    CONST FetchAsNeeded             := "fetchasneeded" AS STRING
    CONST FetchMemo                 := "fetchmemo" AS STRING
    CONST FetchSize                 := "fetchsize" AS STRING
    CONST MaxRecords                := "maxrecords" AS STRING
    CONST Offline                   := "offline" AS STRING
    CONST ParameterList             := "parameterlist" AS STRING
    CONST Prepared                  := "prepared" AS STRING
    //CONST RuleExpression            := "ruleexpression" as string
    //CONST RuleText                  := "ruletext" as string
    CONST SendUpdates               := "sendupdates" AS STRING
    CONST ShareConnection           := "shareconnection" AS STRING
    CONST SourceType                := "sourcetype" AS STRING
    CONST SQL                       := "sql" AS STRING
    CONST Tables                    := "tables" AS STRING
    CONST UpdateType                := "updatetype" AS STRING
    CONST UseMemoSize               := "usememosize" AS STRING
    CONST WhereType                 := "wheretype" AS STRING
                                    
                                    
END CLASS    
