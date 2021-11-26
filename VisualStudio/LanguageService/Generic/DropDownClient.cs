using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;
using XSharpModel;
using MVP = Microsoft.VisualStudio.Package;
using File = System.IO.File;
namespace XSharp.LanguageService
{

    public class XSharpDropDownClient : IVsDropdownBarClient
    {

        static readonly  ImageList _imageList = new ImageList();
        private IVsDropdownBar _dropDownBar;
        Dictionary<ITextView, ITextView> _textViews;
        ITextView _activeView = null;
        IVsDropdownBarManager _manager;
        XSourceEntity _lastSelected = null;
        XSourceEntity _lastType = null;

        List<XDropDownMember> _members = null;
        List<XDropDownMember> _types = null;
        XFile _file = null;
        uint _lastHashCode = 0;
        private int _lastLine;
        private int _selectedMemberIndex = -1;
        private int _selectedTypeIndex = -1;
        private bool _lastIncludeFields;
        private bool _lastCurrentTypeOnly;
        private bool _lastExcludeOtherFiles;
        private bool _lastSorted ;
        private List<string> _relatedFiles;
        private DateTime _lastFileChanged = DateTime.MinValue;

        static XSharpDropDownClient()
        {
            Stream stream = typeof(MVP.LanguageService).Assembly.GetManifestResourceStream("Resources.completionset.bmp");
            _imageList.ImageSize = new Size(16, 16);
            _imageList.TransparentColor = Color.FromArgb(255, 0, 255);
            _imageList.Images.AddStrip(new Bitmap(stream));
        }


        public XSharpDropDownClient(IVsDropdownBarManager manager, XFile file)
        {
            _manager = manager;
            _textViews = new Dictionary<ITextView, ITextView>();
            _file = file;
            if (file != null)
            {
                _file.ContentsChanged += _file_ContentsChanged;
            }
            _types = new List<XDropDownMember>();
            _members = new List<XDropDownMember>();
            _saveSettings();
            _lastLine = 0;
            _relatedFiles = new List<string>();
            _lastFileChanged = DateTime.MinValue;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await RefreshDropDownAsync(needsUI: false);
            });
        }

        internal void addTextView(ITextView textView)
        {
            if (! _textViews.ContainsKey(textView))
            {
                _textViews.Add(textView, textView);
                textView.GotAggregateFocus += TextView_GotAggregateFocus;
                textView.LostAggregateFocus += TextView_LostAggregateFocus;
                textView.Closed += TextView_Closed;
                textView.Caret.PositionChanged += Caret_PositionChanged;

            }
        }

        private void TextView_Closed(object sender, EventArgs e)
        {
            if (sender is ITextView textView)
            {
                if (_textViews.ContainsKey(textView))
                {
                    textView.GotAggregateFocus -= TextView_GotAggregateFocus;
                    textView.LostAggregateFocus -= TextView_LostAggregateFocus;
                    textView.Closed -= TextView_Closed;
                    textView.Caret.PositionChanged -= Caret_PositionChanged;
                    _textViews.Remove(textView);
                    _file.ContentsChanged -= _file_ContentsChanged;

                }
            }
        }

        private void TextView_LostAggregateFocus(object sender, EventArgs e)
        {
            if (sender is ITextView textView &&  textView == _activeView)
            {
                //_activeView = null;
            }
        }

        private void TextView_GotAggregateFocus(object sender, EventArgs e)
        {
            if (sender is ITextView textView && _textViews.ContainsKey(textView))
            {
                _activeView = textView;
                Caret_PositionChanged(textView, new CaretPositionChangedEventArgs(textView, textView.Caret.Position, textView.Caret.Position));
            }

        }

        private void _file_ContentsChanged()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await RefreshDropDownAsync(needsUI: true);
            });
        }

        private void validateSelections()
        {
            if (_selectedTypeIndex == -1)
                _selectedTypeIndex = 0;
            if (_selectedMemberIndex == -1)
                _selectedMemberIndex = 0;
        }
        private void Caret_PositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            int newLine = e.NewPosition.BufferPosition.GetContainingLine().LineNumber;
            if (newLine != _lastLine && _dropDownBar != null)
            {
                SelectContainingMember(newLine);
                _lastLine = newLine;
                validateSelections();
                _dropDownBar.RefreshCombo(0, _selectedTypeIndex);
                _dropDownBar.RefreshCombo(1, _selectedMemberIndex);
            }
        }


        private int findSelectedMember(XSourceEntity member)
        {
            if (member != null)
            {
                for (int i = 0; i < _members.Count; i++)
                {
                    if (String.Compare(_members[i].Entity.FullName, member.FullName, true) == 0)
                        return i;
                }
            }
            return -1;
        }
        private bool relatedFilesChanged
        {
            get 
            {
                // Check if we have other files with source that is shown in this editor
                // and check their timestamps.
                if (_relatedFiles.Count > 1)
                {
                    foreach (var file in _relatedFiles)
                    {
                        var dt = File.GetLastWriteTime(file);
                        if (dt > _lastFileChanged)
                        {
                            return true;
                        }
                    }
                }
                return false;
            }
        }

        private int findSelectedType(XSourceTypeSymbol type)
        {
            if (type != null)
            {
                for (int i = 0; i < _types.Count; i++)
                {
                    if (String.Compare(_types[i].Entity.FullName, type.FullName, true) == 0)
                    {
                        return i;
                    }
                }
            }
            return -1;
        }
        private void SelectContainingMember(int newLine)
        {
            if (_file == null)
                return;
            XSourceEntity selectedElement = _file.FindMemberAtRow(newLine);
            if (selectedElement == _lastSelected && !SettingsChanged)
            {
                return;
            }
            XSourceTypeSymbol parentType = null;
            if (selectedElement is XSourceMemberSymbol)
            {
                parentType = (XSourceTypeSymbol)selectedElement.Parent;
            }
            else if (selectedElement is XSourceTypeSymbol)
            {
                parentType = selectedElement as XSourceTypeSymbol;
            }
            else
            {
                parentType = _file.GlobalType;
            }
            bool newType = true;
            if (parentType != null && _lastType != null && parentType.FullName == _lastType.FullName)
            {
                newType = false;
            }
            var nothingChanged = _file.ContentHashCode == this._lastHashCode && !SettingsChanged && !relatedFilesChanged;
            if (nothingChanged)
            {
                if (newType && XSettings.EditorNavigationMembersOfCurrentTypeOnly)
                {
                    _selectedTypeIndex = findSelectedType(parentType);
                    loadMemberCombos(_selectedTypeIndex, newLine);
                    _lastHashCode = _file.ContentHashCode;
                    _lastType = parentType;
                    _saveSettings();
                    return;
                }
                // when not on a new type or when the member combo is already complete then we
                // can select the entry in the existing members combobox
                _selectedMemberIndex = findSelectedMember(selectedElement);
                _selectedTypeIndex = findSelectedType(parentType);
                _lastSelected = selectedElement;
                _lastType = parentType;
            }
            else
            {
                reloadCombos(newLine);
            }
            return;
        }

        private IList<XSourceEntity> GetTypeMembers(XSourceTypeSymbol type)
        {
            var members = new List<XSourceEntity>();
            if (type.IsPartial && ! type.IsGlobal && !XSettings.EditorNavigationExcludeMembersFromOtherFiles)
            { 
                var usings = new List<string>();
                usings.Add(type.Namespace);
                var fullType = _file.Project.Lookup(type.Name, usings);
                if (fullType != null)
                { 
                    members.AddRange(fullType.XMembers);
                }
            }
            else
            {
               
                members.AddRange(type.XMembers);
                foreach (XSourceTypeSymbol child in type.Children)
                {
                    members.Add(child);
                    members.AddRange(child.XMembers);
                }
            }
            return members;
        }

        private void AddSourceFile(string fileName)
        {
            var changed = File.GetLastWriteTime(fileName);
            fileName = fileName.ToLower();
            if (!_relatedFiles.Contains(fileName))
            {
                _relatedFiles.Add(fileName);
                if (changed > this._lastFileChanged)
                { 
                    _lastFileChanged = changed;
                }
            }
            
        }

        private IList<XSourceEntity> GetAllMembers()
        {
            var members = new List<XSourceEntity>();
            var includeFields = XSettings.EditorNavigationIncludeFields;
            members.AddRange(_file.EntityList.Where(member => includeFields || !member.Kind.IsField()));
            _lastFileChanged = DateTime.MinValue;
            foreach (var ent in _file.EntityList)
            {
                if (ent is XSourceTypeSymbol)
                {
                    var xType = ent as XSourceTypeSymbol;
                    if (xType.IsPartial && !XSettings.EditorNavigationExcludeMembersFromOtherFiles)
                    {

                        // load methods from other files
                        var usings = new List<string>();
                        usings.Add(xType.Namespace);
                        var fullType = _file.Project.Lookup(xType.Name, usings);
                        if (fullType != null)
                        {
                            foreach (var member in fullType.XMembers)
                            {
                                if (member.File != null && string.Compare(member.File.FullPath, _file.FullPath, true) != 0)
                                {
                                    if (includeFields || (member.Kind != Kind.Field && member.Kind != Kind.VODefine))
                                    {
                                        members.Add(member);
                                    }
                                }
                            }
                        }

                    }
                }
            }
            return members;
        }

        private void loadTypeCombos(int nLine)
        {
            _types.Clear();
            var sortItems = XSettings.EditorNavigationSorted;
            XDropDownMember elt;
            int nSelect = 0;
            DROPDOWNFONTATTR ft;
            List<XSourceTypeSymbol> xList = _file.TypeList.Values.ToList<XSourceTypeSymbol>();
            if (sortItems)
            {
                xList.Sort(delegate (XSourceTypeSymbol elt1, XSourceTypeSymbol elt2)
                {
                    return elt1.FullName.CompareTo(elt2.FullName);
                });
            }
            if (xList.Count == 0)
            {
                xList.Add(_file.GlobalType);
            }
            foreach (XSourceTypeSymbol eltType in xList)
            {
                if (eltType.Kind == Kind.Namespace)
                    continue;
                //
                TextSpan sp = this.TextRangeToTextSpan(eltType.Range);
                ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                string name = eltType.FullName;
                if (string.IsNullOrEmpty(name))
                {
                    name = "?";
                }
                elt = new XDropDownMember(name, sp, eltType.Glyph, ft, eltType);
                nSelect = _types.Count;
                _types.Add(elt);
                if (eltType.Range.StartLine <= nLine && eltType.Range.EndLine >= nLine)
                {
                    _selectedTypeIndex = nSelect;
                    _lastType = eltType;
                }
            }
            return ;
        }



        private void loadMemberCombos(int selectedType, int nLine)
        {
            var sortItems = XSettings.EditorNavigationSorted;
            var includeFields = XSettings.EditorNavigationIncludeFields;
            var currentTypeOnly = XSettings.EditorNavigationMembersOfCurrentTypeOnly;
            if (selectedType >= _types.Count)
                return;
            var currentType = (XSourceTypeSymbol) (selectedType > -1 ? _types[selectedType].Entity : _types[0].Entity);
            var globalType = _file.GlobalType;
            DROPDOWNFONTATTR ft;
            bool hasPartial = !currentType.IsGlobal && currentType.IsPartial;
            _members.Clear();
            _relatedFiles.Clear();
            AddSourceFile(_file.SourcePath);
            var members = new List<XSourceEntity>();
            if (currentTypeOnly)
            {
                members.AddRange(GetTypeMembers(currentType));
            }
            else
            {
                // get members of types in this file and members of partial types in other files.
                members.AddRange(GetAllMembers());
            }
            if (sortItems)
            {
                members = members.OrderBy(x => x.FullName).ToList();
            }
            // now either load all members or just the members of the current type
            // Add member for class declaration. This also makes sure that there at least one
            // element in the members list, which is convenient.
            TextSpan spM = this.TextRangeToTextSpan(currentType.Range);
            ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
            XDropDownMember elt;
            if (currentTypeOnly)
            {
                // Add a 'root' element for the type.
                if (currentType != globalType )
                {
                    if (currentType.Kind != Kind.Delegate)
                    {
                        elt = new XDropDownMember("(" + currentType.Name + ")", spM, currentType.Glyph, ft, currentType);
                        _members.Add(elt);
                    }
                }
                else
                {
                    elt = new XDropDownMember(currentType.Name, spM, currentType.Glyph, ft, currentType);
                    _members.Add(elt);
                }
            }
            foreach (XSourceEntity member in members)
            {
                bool otherFile;
                if (includeFields || (member.Kind != Kind.Field && member.Kind != Kind.VODefine))
                {
                    spM = this.TextRangeToTextSpan(member.Range);
                    otherFile = false;
                    ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                    if (member.Parent is XSourceTypeSymbol typedef &&  typedef.IsPartial)
                    {
                        otherFile = string.Compare(member.File.FullPath, _file.FullPath, true) != 0;
                    }

                    string prototype = member.ComboPrototype;
                    bool addPrefix = false;
                    if (currentTypeOnly)
                    {
                        addPrefix = false;
                    }
                    else
                    {
                        if (member.Parent is XSourceEntity && member.Parent.Name != XLiterals.GlobalName && member.Kind.IsClassMember(_file.Project.Dialect))
                        {
                            addPrefix = true;
                        }
                    }
                    if (addPrefix)
                    {
                        var parent = member.Parent as XSourceEntity;
                        if (member.Modifiers.HasFlag(Modifiers.Static) || member is XSourceTypeSymbol)
                            prototype = parent.ComboPrototype + "." + prototype;
                        else
                            prototype = parent.ComboPrototype + ":" + prototype;
                    }
                    if (member.Kind.IsLocal())
                    {
                        prototype = member.Parent.Name + "." + prototype;
                    }
                    if (otherFile)
                    {
                        AddSourceFile(member.File.SourcePath);
                        ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                        prototype += " (" + System.IO.Path.GetFileName(member.File.SourcePath) + ")";
                    }
                    elt = new XDropDownMember(prototype, spM, member.Glyph, ft, member);
                    var nSelect = _members.Count;
                    _members.Add(elt);
                    if (member.Range.ContainsInclusive(nLine, 0))
                    {
                        _selectedMemberIndex = nSelect;
                        _lastSelected = member;
                    }
                }
            }

        }

        private void reloadCombos(int nLine)
        {
            loadTypeCombos(nLine);
            loadMemberCombos(_selectedTypeIndex, nLine);
            _lastHashCode = _file.ContentHashCode;
            _saveSettings();
        }

        private TextSpan TextRangeToTextSpan(TextRange tr)
        {
            // It seems that TextSpan is Zero-based
            // where our TextRange is One-Based.
            // --> Make the move
            TextSpan ts = new TextSpan();
            ts.iStartLine = tr.StartLine;
            ts.iStartIndex = tr.StartColumn;
            ts.iEndLine = tr.EndLine;
            ts.iEndIndex = tr.EndColumn;
            // validate values
            if (ts.iStartLine < 0)
                ts.iStartLine = 0;
            if (ts.iStartIndex < 0)
                ts.iStartIndex = 0;
            if (ts.iEndLine < ts.iStartLine)
                ts.iEndLine = ts.iStartLine;
            if (ts.iEndIndex < ts.iStartLine && ts.iStartLine == ts.iEndLine)
                ts.iEndIndex = ts.iStartIndex;
            return ts;
        }
        private void _saveSettings()
        {
            _lastSorted = XSettings.EditorNavigationSorted;
            _lastIncludeFields = XSettings.EditorNavigationIncludeFields;
            _lastCurrentTypeOnly = XSettings.EditorNavigationMembersOfCurrentTypeOnly;
            _lastExcludeOtherFiles = XSettings.EditorNavigationExcludeMembersFromOtherFiles;
        }
        bool SettingsChanged => _lastSorted != XSettings.EditorNavigationSorted ||
            _lastIncludeFields != XSettings.EditorNavigationIncludeFields ||
            _lastCurrentTypeOnly != XSettings.EditorNavigationMembersOfCurrentTypeOnly ||
            _lastExcludeOtherFiles != XSettings.EditorNavigationExcludeMembersFromOtherFiles;

        public async System.Threading.Tasks.Task RefreshDropDownAsync(bool needsUI)
        {
            try
            {
                if (_dropDownBar != null && _file != null )
                {
                    if (_file.ContentHashCode != _lastHashCode  || SettingsChanged)
                    {
                        reloadCombos(_lastLine);
                    }
                    if (_activeView != null)
                    {
                        int caretPosition = _activeView.Caret.Position.BufferPosition.Position;
                        if (!needsUI)
                        {
                            SelectContainingMember(caretPosition);
                            return;
                        }
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        SelectContainingMember(caretPosition);
                    }
                }
            }
            catch (Exception)
            {
            }
        }

        public int GetComboAttributes(int combo, out uint entries, out uint entryType, out IntPtr imageList)
        {
            entries = 0;
            imageList = _imageList.Handle;
            entryType = (uint)(DROPDOWNENTRYTYPE.ENTRY_ATTR | DROPDOWNENTRYTYPE.ENTRY_IMAGE | DROPDOWNENTRYTYPE.ENTRY_TEXT);
            switch (combo)
            {
                case 0: // types
                    entries = (uint) _types.Count;
                    break;
                case 1: // members
                    entries = (uint)_members.Count;
                    break;
                default:
                    return VSConstants.E_INVALIDARG;
            }
            return VSConstants.S_OK;
        }

        public int GetComboTipText(int combo, out string text)
        {
            int selectedIndex = 0;
            return GetEntryText(combo, selectedIndex, out text);
        }

        public int GetEntryAttributes(int combo, int index, out uint attr)
        {
            attr = 0;
            if (index < 0)
                index = 0;

            switch (combo)
            {
                case 0:
                    if (index < _types.Count)
                        attr = (uint)_types[index].FontAttr;
                    break;
                case 1:
                    if (index < _members.Count)
                        attr = (uint) _members[index].FontAttr;
                    break;
                default:
                    return VSConstants.E_INVALIDARG;
            }
            return VSConstants.S_OK;
        }

        public int GetEntryImage(int combo, int index, out int imageIndex)
        {

            imageIndex = 0;
            if (index < 0)
                throw new ArgumentOutOfRangeException();
            switch (combo)
            {
                case 0: // types
                    if (index < _types.Count)
                        imageIndex = _types[index].Glyph;
                    break;
                case 1: // members
                    if (index < _members.Count)
                        imageIndex = _members[index].Glyph;
                    break;
                default:
                    throw new ArgumentOutOfRangeException();

            }
            return 0;
        }

        public int GetEntryText(int combo, int index, out string text)
        {
            text = "";
            if (index < 0)
                index = 0;
            switch (combo)
            {
                case 0: // types
                    if (index < _types.Count)
                        text = _types[index].Label;
                    break;
                case 1: // members
                    if (index < _members.Count)
                        text = _members[index].Label;
                    break;
                default:
                    return VSConstants.E_INVALIDARG;

            }
            return VSConstants.S_OK;
        }

        public int OnComboGetFocus(int combo)
        {

            return VSConstants.S_OK;
        }

        public int OnItemChosen(int combo, int index)
        {
            XSourceEntity entity = null;
            if (index < 0)
                index = 0;
            switch (combo)
            {
                case 0:
                    if (index < _types.Count)
                        entity = _types[index].Entity;
                    break;
                case 1:
                    if (index < _members.Count)
                        entity = _members[index].Entity;
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            if (entity != null)
            {
                bool open = false;
                if (entity.File.FullPath == this._file.FullPath)
                {
                    IVsTextView textView = ThreadHelper.JoinableTaskFactory.Run(GetActiveTextViewAsync); 
                    if (textView != null)
                    {
                        textView.SetCaretPos(entity.Range.StartLine, 0);
                        if (entity.Range.StartLine > 5)
                            textView.SetTopLine(entity.Range.StartLine - 5);
                        else
                            textView.SetTopLine(0);
                        textView.SendExplicitFocus();
                        open = true;
                    }
                }
                if (! open)
                {
                    entity.OpenEditor();
                }
            }
            return VSConstants.S_OK;
        }

        private async Task<IVsTextView> GetActiveTextViewAsync( )
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            IVsTextManager textManager = await ServiceProvider.GetGlobalServiceAsync<SVsTextManager, IVsTextManager>();
            ErrorHandler.ThrowOnFailure(textManager.GetActiveView(1, null, out IVsTextView activeView));
            return activeView;
        }

        public int SetDropdownBar(IVsDropdownBar dropdownBar)
        {
            _dropDownBar = dropdownBar;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await RefreshDropDownAsync(needsUI: false);
            });
            if (_dropDownBar != null)
            {
                _dropDownBar.RefreshCombo(0, 1);
                _dropDownBar.RefreshCombo(1, 1);
            }
            return VSConstants.S_OK;
        }

        public int OnItemSelected(int iCombo, int iIndex)
        {
            if (iCombo == 0)
                _selectedTypeIndex = iIndex;
            if (iCombo == 1)
                _selectedMemberIndex = iIndex;
            return VSConstants.S_OK;
        }

        [DebuggerDisplay("{Label}")]
        internal class XDropDownMember
        {
            public TextSpan Span { get; set; }
            public string Label { get; set; }
            public int Glyph { get; set; }
            public DROPDOWNFONTATTR FontAttr { get; set; }
            public XSourceEntity Entity { get; set; }
            internal XDropDownMember(string label, TextSpan span, int glyph, DROPDOWNFONTATTR fontAttribute, XSourceEntity element)
            {
                Label = label;
                Span = span;
                Glyph = glyph;
                FontAttr = fontAttribute;
                Entity = element;
            }
        }

        
    }
}
