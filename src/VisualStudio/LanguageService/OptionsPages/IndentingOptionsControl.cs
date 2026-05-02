// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class IndentingOptionsControl : XSUserControl
    {
        private readonly ObservableCollection<IndentItem> _items = new ObservableCollection<IndentItem>();

        private static readonly string RtfPrefix =
            @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}";

        public IndentingOptionsControl()
        {
            InitializeComponent();
            _items.Add(new IndentItem("IndentNamespace",
                "Indent entities inside namespace",
                @"\cf1 BEGIN NAMESPACE \cf0 MyNs\par\cf1{tab} CLASS \cf0 MyClass \par \par {tab} \cf1 END CLASS \cf0\par\cf1 END NAMESPACE\par}"));
            _items.Add(new IndentItem("IndentEntityContent",
                "Indent multiline members inside types",
                @"\cf1 CLASS \cf0 foo\par\par\cf1{tab} METHOD \cf0 m1() \cf1 AS VOID\cf0\par\par\cf1{tab} CONSTRUCTOR \cf0 m1()\cf0\par}"));
            _items.Add(new IndentItem("IndentFieldContent",
                "Indent single line members inside types",
                @"\cf1 CLASS \cf0 foo\par\cf1{tab} PUBLIC \cf0 x \cf1 AS INT\cf0\par\par\cf1{tab} PROPERTY \cf0 y \cf1 AS STRING AUTO \cf0\par}"));
            _items.Add(new IndentItem("IndentBlockContent",
                "Indent statements inside entities",
                @"\cf1 FUNCTION \cf0 foo\par\cf1{tab} LOCAL \cf0 x \cf1 AS INT\cf0\par\cf1{tab} LOCAL \cf0 y \cf1 AS INT\cf0\par}"));
            _items.Add(new IndentItem("IndentCaseLabel",
                "Indent case label",
                @"\cf1 DO CASE \par\cf1{tab} CASE \cf0 x == 1\par\par\cf1{tab} CASE \cf0 x == 2\par}"));
            _items.Add(new IndentItem("IndentCaseContent",
                "Indent statements inside case block",
                @"\cf1 DO CASE \par\cf1\tab CASE \cf0 x == 1\par\cf1\tab{tab} nop\par\cf1\tab{tab} nop\cf0\par}"));
            _items.Add(new IndentItem("IndentMultiLines",
                "Indent continuing lines",
                @"\cf1 FUNCTION \cf0 foo( \cf0 x \cf1 AS INT ; \par{tab}\cf0 y \cf1 AS INT \cf0 ;\par{tab} \cf0 z \cf1 AS INT \cf0) \par}"));
            _items.Add(new IndentItem("IndentPreprocessorLines",
                "Indent preprocessor lines",
                @"\cf1 CLASS \cf0 foo\par{tab}\cf0#region FIELDS \par\cf1\tab PUBLIC \cf0 x \cf1 AS INT\cf0\par{tab} \cf0#endregion\par\cf1\tab METHOD \cf0 m1() \cf1 AS VOID\cf0\par}"));

            foreach (var item in _items)
                item.PropertyChanged += (s, e) =>
                {
                    if (e.PropertyName == nameof(IndentItem.IsChecked))
                        ShowCodeSample(_listView.SelectedItem as IndentItem);
                };

            _listView.ItemsSource = _items;
            if (_items.Count > 0)
                _listView.SelectedIndex = 0;
        }

        private void OnSelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            ShowCodeSample(_listView.SelectedItem as IndentItem);
        }

        private void ShowCodeSample(IndentItem item)
        {
            if (item == null) return;
            var rtf = item.RtfCode;
            if (item.IsChecked)
                rtf = rtf.Replace("{tab}", @"\tab");
            else
                rtf = rtf.Replace("{tab}", "");
            if (rtf.EndsWith("}"))
                rtf = RtfPrefix + rtf;
            SetRtf(rtf);
        }

        private void SetRtf(string rtf)
        {
            try
            {
                var bytes = Encoding.ASCII.GetBytes(rtf);
                using (var ms = new MemoryStream(bytes))
                {
                    var range = new TextRange(codeSample.Document.ContentStart, codeSample.Document.ContentEnd);
                    range.Load(ms, DataFormats.Rtf);
                }
            }
            catch
            {
                // ignore RTF parsing errors
            }
        }

        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            var opts = (IndentingOptions)options;
            foreach (var item in _items)
            {
                var prop = typeof(IndentingOptions).GetProperty(item.PropertyName);
                if (prop?.GetValue(opts) is bool b)
                    item.IsChecked = b;
            }
            ShowCodeSample(_listView.SelectedItem as IndentItem);
        }

        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
            var opts = (IndentingOptions)options;
            foreach (var item in _items)
            {
                var prop = typeof(IndentingOptions).GetProperty(item.PropertyName);
                prop?.SetValue(opts, item.IsChecked);
            }
        }
    }

    internal sealed class IndentItem : INotifyPropertyChanged
    {
        private bool _isChecked;

        public string PropertyName { get; }
        public string Label { get; }
        public string RtfCode { get; }

        public bool IsChecked
        {
            get => _isChecked;
            set
            {
                if (_isChecked != value)
                {
                    _isChecked = value;
                    OnPropertyChanged();
                }
            }
        }

        public IndentItem(string propertyName, string label, string rtfCode)
        {
            PropertyName = propertyName;
            Label = label;
            RtfCode = rtfCode;
        }

        public event PropertyChangedEventHandler PropertyChanged;
        private void OnPropertyChanged([CallerMemberName] string name = null)
            => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
    }
}
