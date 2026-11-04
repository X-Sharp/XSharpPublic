//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Drawing;
using System.IO;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.NavigateTo.Interfaces;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.LanguageService.NavigateTo
{
    internal sealed class NavigateToDisplayFactory : INavigateToItemDisplayFactory
    {
        internal static readonly NavigateToDisplayFactory Instance = new NavigateToDisplayFactory();

        public INavigateToItemDisplay CreateItemDisplay(NavigateToItem item)
        {
            return new NavigateToDisplay((NavigateToEntry)item.Tag);
        }
    }

    /// <summary>A result in Go To All: name, the declaring type, the file and the project; Enter opens the declaration.</summary>
    internal sealed class NavigateToDisplay : INavigateToItemDisplay3
    {
        private static readonly ReadOnlyCollection<DescriptionItem> _noDescription =
            new ReadOnlyCollection<DescriptionItem>(Array.Empty<DescriptionItem>());
        private readonly NavigateToEntry _entry;

        internal NavigateToDisplay(NavigateToEntry entry)
        {
            _entry = entry;
        }

        public string Name => _entry.Name;

        public string AdditionalInformation
        {
            get
            {
                var file = Path.GetFileName(_entry.FilePath);
                var where = string.IsNullOrEmpty(_entry.ProjectName) ? file : $"{file}, {_entry.ProjectName}";
                return string.IsNullOrEmpty(_entry.Container) ? where : $"{_entry.Container} ({where})";
            }
        }

        public string Description => null;
        public ReadOnlyCollection<DescriptionItem> DescriptionItems => _noDescription;
        public Icon Glyph => null;
        public ImageMoniker GlyphMoniker => _entry.Kind.GetImageMoniker(_entry.Visibility);

        public void NavigateTo()
        {
            XDocuments.Open(_entry.FilePath, _entry.Line, _entry.Column, false);
        }

        public int GetProvisionalViewingStatus()
        {
            return (int)__VSPROVISIONALVIEWINGSTATUS.PVS_Enabled;
        }

        public void PreviewItem()
        {
            XDocuments.Open(_entry.FilePath, _entry.Line, _entry.Column, true);
        }

        public IReadOnlyList<Span> GetNameMatchRuns(string searchValue) => Array.Empty<Span>();
        public IReadOnlyList<Span> GetAdditionalInformationMatchRuns(string searchValue) => Array.Empty<Span>();
    }
}
