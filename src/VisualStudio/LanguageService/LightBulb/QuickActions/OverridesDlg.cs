//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>The overridable members of the base classes, to tick the ones to generate.</summary>
    internal sealed class OverridesDlg : Form
    {
        private readonly CheckedListBox _list = new CheckedListBox();
        private readonly CheckBox _all = new CheckBox();
        private readonly IReadOnlyList<IXMemberSymbol> _members;

        internal OverridesDlg(string className, IReadOnlyList<IXMemberSymbol> members)
        {
            _members = members;
            Text = "Generate overrides";
            StartPosition = FormStartPosition.CenterParent;
            FormBorderStyle = FormBorderStyle.Sizable;
            MinimizeBox = false;
            MaximizeBox = false;
            ShowInTaskbar = false;
            ClientSize = new Size(560, 460);
            MinimumSize = new Size(360, 260);
            Padding = new Padding(10);

            var caption = new Label
            {
                Text = $"Members to override in class '{className}':",
                Dock = DockStyle.Top,
                AutoSize = false,
                Height = 24,
            };
            _list.Dock = DockStyle.Fill;
            _list.CheckOnClick = true;
            _list.IntegralHeight = false;
            foreach (var member in members)
                _list.Items.Add(Display(member));

            _all.Text = "Select all";
            _all.AutoSize = true;
            _all.Dock = DockStyle.Left;
            _all.CheckedChanged += (s, e) =>
            {
                for (int i = 0; i < _list.Items.Count; i++)
                    _list.SetItemChecked(i, _all.Checked);
            };
            var ok = new Button { Text = "OK", DialogResult = DialogResult.OK, Dock = DockStyle.Right, Width = 90 };
            var cancel = new Button { Text = "Cancel", DialogResult = DialogResult.Cancel, Dock = DockStyle.Right, Width = 90 };
            var bottom = new Panel { Dock = DockStyle.Bottom, Height = 34, Padding = new Padding(0, 8, 0, 0) };
            bottom.Controls.Add(_all);
            bottom.Controls.Add(ok);
            bottom.Controls.Add(new Panel { Dock = DockStyle.Right, Width = 8 });
            bottom.Controls.Add(cancel);
            // dock order: the last added control docks first
            cancel.BringToFront();

            Controls.Add(_list);
            Controls.Add(caption);
            Controls.Add(bottom);
            AcceptButton = ok;
            CancelButton = cancel;
        }

        /// <summary>The ticked members, in list order.</summary>
        internal IList<IXMemberSymbol> Selected =>
            _list.CheckedIndices.Cast<int>().OrderBy(i => i).Select(i => _members[i]).ToList();

        private static string Display(IXMemberSymbol member)
        {
            var owner = (member.Parent as IXTypeSymbol)?.Name ?? member.ParentType?.Name ?? "";
            var kind = member.Kind == Kind.Method ? "" : member.Kind.ToString().ToUpperInvariant() + " ";
            var parameters = member.Kind == Kind.Access || member.Kind == Kind.Property
                ? ""
                : "(" + string.Join(", ", (member.Parameters ?? new List<IXParameterSymbol>()).Select(p => p.TypeName)) + ")";
            return $"{owner}: {kind}{member.Name}{parameters}";
        }
    }
}
