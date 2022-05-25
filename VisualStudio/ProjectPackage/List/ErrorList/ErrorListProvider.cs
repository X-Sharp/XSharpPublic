//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;

namespace XSharp.Project
{
    [Export(typeof(ErrorListProvider))]
    internal class ErrorListProvider : ListProvider
    {
        internal ErrorListProvider(ITableManager manager) : base(manager)
        {
        }
        public override string SourceTypeIdentifier => StandardTableDataSources.ErrorTableDataSource;

        public override IReadOnlyCollection<string> Columns { get; } = new[]
        {
            StandardTableColumnDefinitions.DetailsExpander,
            StandardTableColumnDefinitions.ErrorCategory,
            StandardTableColumnDefinitions.ErrorSeverity,
            StandardTableColumnDefinitions.ErrorCode,
            StandardTableColumnDefinitions.ErrorSource,
            StandardTableColumnDefinitions.BuildTool,
            StandardTableColumnDefinitions.Text,
            StandardTableColumnDefinitions.DocumentName,
            StandardTableColumnDefinitions.Line,
            StandardTableColumnDefinitions.Column,
            StandardTableColumnDefinitions.ProjectName
        };
    }
}
