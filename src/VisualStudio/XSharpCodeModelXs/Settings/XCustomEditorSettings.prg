﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp.Settings
    STATIC CLASS XCustomEditorSettings
        STATIC PROPERTY ShowGrid AS LOGIC AUTO := TRUE
        STATIC PROPERTY GridX AS INT AUTO   := 8
        STATIC PROPERTY GridY AS INT AUTO   := 8

        STATIC PROPERTY PasteOffSetX AS LONG AUTO := 8
        STATIC PROPERTY PasteOffSetY AS LONG AUTO := 8
        STATIC PROPERTY PartialLasso AS LOGIC AUTO := FALSE
        STATIC PROPERTY SizeAdjustmentX AS REAL8 AUTO := 1.0
        STATIC PROPERTY SizeAdjustmentY AS REAL8 AUTO := 1.0

        STATIC PROPERTY DbServerParentClass AS STRING AUTO := "DbServer"
        STATIC PROPERTY DbServerDefaultRDD AS STRING AUTO := "DBFNTX"
        STATIC PROPERTY FieldSpecParentClass AS STRING AUTO := "FieldSpec"
        STATIC PROPERTY MenuParentClass AS STRING AUTO := "Menu"
        STATIC PROPERTY ToolbarParentClass AS STRING AUTO := "Menu"
        STATIC PROPERTY BackupFormFiles AS LOGIC AUTO := FALSE

    END CLASS

END NAMESPACE
