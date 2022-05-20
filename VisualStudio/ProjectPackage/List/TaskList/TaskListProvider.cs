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
    [Export(typeof(TaskListProvider))]
    internal class TaskListProvider : ListProvider
    {

        internal TaskListProvider(ITableManager manager) : base(manager)
        {
        }
        public override string SourceTypeIdentifier => StandardTableDataSources.CommentTableDataSource;
        public override IReadOnlyCollection<string> Columns { get; } = new[]
        {
            StandardTableColumnDefinitions.Priority,
            StandardTableColumnDefinitions.Text,
            StandardTableColumnDefinitions.ProjectName,
            StandardTableColumnDefinitions.DocumentName,
            StandardTableColumnDefinitions.Line,
            StandardTableColumnDefinitions.Column
        };

    }
}
