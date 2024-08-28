﻿using Microsoft.VisualStudio;
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
using File = System.IO.File;
using MVP = Microsoft.VisualStudio.Package;
using Task = System.Threading.Tasks.Task;
using XSharp.Settings;
namespace XSharp.LanguageService
{
    internal class DropdownSettings
    {
        internal bool CurrentTypeOnly { get; private set; }
        internal bool IncludeFields { get; private set; }
        internal bool Sorted { get; private set; }
        internal bool ExcludeOtherFiles { get; private set; }

        internal DropdownSettings()
        {
            CurrentTypeOnly = XEditorSettings.NavigationMembersOfCurrentTypeOnly;
            IncludeFields = XEditorSettings.NavigationIncludeFields;
            Sorted = XEditorSettings.NavigationSorted;
            ExcludeOtherFiles = XEditorSettings.NavigationExcludeMembersFromOtherFiles;
        }
        internal bool Changed
        {
            get
            {
                return CurrentTypeOnly != XEditorSettings.NavigationMembersOfCurrentTypeOnly ||
                    IncludeFields != XEditorSettings.NavigationIncludeFields ||
                    Sorted != XEditorSettings.NavigationSorted ||
                    ExcludeOtherFiles != XEditorSettings.NavigationExcludeMembersFromOtherFiles;
            }

        }

    }

    public class XSharpDropDownClient : IVsDropdownBarClient
    {
        const int PROJECTINDEX = 0;
        const int TYPEINDEX = 1;
        const int MEMBERINDEX = 2;

        static readonly ImageList _imageList = new ImageList();
        private IVsDropdownBar _dropDownBar;
        readonly Dictionary<ITextView, ITextView> _textViews;
        ITextView _activeView = null;
        readonly IVsDropdownBarManager _manager;
#if XDEBUG
        XSourceEntity _lastSelected = null;
#endif
        XSourceEntity _lastType = null;
        readonly List<XDropDownMember> _members = null;
        readonly Dictionary<string, int> _membersDict = null;	// to speed up the lookup of members
        readonly List<XDropDownMember> _types = null;
        readonly List<XProject> _projects = null;   
        XFile _file = null;
        uint _lastHashCode = 0;
        private int _lastLine;
        private int _selectedMemberIndex = -1;
        private int _selectedTypeIndex = -1;
        private int _selectedProjectIndex = -1;
        private DropdownSettings _settings;
        private List<string> _relatedFiles;
        private DateTime _lastFileChanged = DateTime.MinValue;
        private static readonly int projectIcon = -1;
        private XDocument _document;
        XProject ActiveProject
        {
            get
            {
                if (_selectedProjectIndex >= 0 && _selectedProjectIndex < _projects.Count)
                {
                    return _projects[_selectedProjectIndex];
                }
                return null;
            }
        }

        static XSharpDropDownClient()
        {
            // Load images from Microsoft and Our project system (linked resource file)
            Stream stream = typeof(MVP.LanguageService).Assembly.GetManifestResourceStream("Resources.completionset.bmp");
            _imageList.ImageSize = new Size(16, 16);
            _imageList.TransparentColor = Color.FromArgb(255, 0, 255);
            _imageList.Images.AddStrip(new Bitmap(stream));
            // the project Icon is the first in the ImageList from the project package
            projectIcon = _imageList.Images.Count;
            stream = typeof(XSharpDropDownClient).Assembly.GetManifestResourceStream("XSharp.LanguageService.Resources.XSharpProjectImageList.bmp");
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
            _projects = new List<XProject>
            {
                _file.Project
            };
            var projects = XDatabase.GetProjectsPerFile(_file);
            if (projects?.Count > 1)
            {
#if DEBUG
                Logger.Information($"DropdownClient: Found {projects.Count} projects for file {_file.FullPath}");
#endif
                foreach (var proj in projects)
                {
                    if (proj != _file.Project.Id)
                    {
                        var project = XSolution.FindProject(proj);
                        if (project != null)
                            _projects.Add(project);
                    }
                }
            }

            if (_projects.Count > 1)
            {
                _projects.Sort( (a, b) => string.Compare(a.DisplayName, b.DisplayName, true));
                _selectedProjectIndex = _projects.IndexOf(_file.Project);
            }
            else
            {
                _selectedProjectIndex = 0;
            }
            _membersDict = new Dictionary<string, int>();
            _saveSettings();
            _lastLine = -1;
            _relatedFiles = new List<string>();
            _lastFileChanged = DateTime.MinValue;
            ThreadHelper.JoinableTaskFactory.Run(async ()=>
            {
                await RefreshDropDownAsync(needsUI: false);
            });
        }

        internal void addTextView(ITextView textView, IVsTextView textViewAdapter)
        {
            if (!_textViews.ContainsKey(textView))
            {
                _textViews.Add(textView, textView);
                this._document = textView.TextBuffer.GetDocument();
                _file.Project = ActiveProject;
                textView.GotAggregateFocus += TextView_GotAggregateFocus;
                textView.LostAggregateFocus += TextView_LostAggregateFocus;
                textView.Closed += TextView_Closed;
                StartOnIdleAsync(textViewAdapter).FireAndForget();
                

            }
        }
        // This moves the caret to trigger initial drop down load
        private Task StartOnIdleAsync(IVsTextView textView)
        {
#if DEV17
            return ThreadHelper.JoinableTaskFactory.StartOnIdle(() =>
            {
                InitCaret(textView);
            }).Task;
#else
            return ThreadHelper.JoinableTaskFactory.StartOnIdleShim(() =>
            {
                InitCaret(textView);
            }).Task;
#endif
        }
        void InitCaret(IVsTextView textView)
        {
            if (_textViews.Count > 0 && _activeView != null)
            {
                textView.SendExplicitFocus();
                var pos = _activeView.Caret.Position;
                _activeView.Caret.MoveToNextCaretPosition();
                _activeView.Caret.PositionChanged += Caret_PositionChanged;
                _activeView.Caret.MoveTo(pos.BufferPosition);
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
                    if (_activeView == textView)
                        _activeView = null;
                    _textViews.Remove(textView);
                    _file.ContentsChanged -= _file_ContentsChanged;
                }
            }
        }

        private void TextView_LostAggregateFocus(object sender, EventArgs e)
        {
            if (sender is ITextView textView && textView == _activeView)
            {
                //_activeView = null;
            }
        }

        private void TextView_GotAggregateFocus(object sender, EventArgs e)
        {
            if (sender is ITextView textView && _textViews.ContainsKey(textView))
            {
                _activeView = textView;
                LineChanged();
            }
        }

        private void _file_ContentsChanged()
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await RefreshDropDownAsync(needsUI: true);
                LineChanged();
            });
        }

        private void LineChanged()
        {
            if (_activeView != null)
                Caret_PositionChanged(_activeView, new CaretPositionChangedEventArgs(_activeView, _activeView.Caret.Position, _activeView.Caret.Position));
        }

        private void Caret_PositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            int newLine = e.NewPosition.BufferPosition.GetContainingLine().LineNumber;
            if (newLine != _lastLine && _dropDownBar != null && _members.Count > 0)
            {
                SelectContainingMember(newLine);
                _lastLine = newLine;
            }
#if XDEBUG
            Logger.Information($"DropdownClient: Caret_PositionChanged {newLine} Types: {_types.Count} Members: {_members.Count}");
            Logger.Information($"DropdownClient: Caret_PositionChanged {newLine} Entity: {_lastSelected} Type: {_selectedTypeIndex}, Member: {_selectedMemberIndex} ");
#endif
            if (_lastLine != -1)
            {
                refreshCombos();
            }
        }


        private int findSelectedMember(XSourceEntity member)
        {
            if (member != null)
            {
                var prot = _getUniqueName(member);
                if (_membersDict.TryGetValue(prot, out var memLoc))
                {
                    return memLoc;
                }
                // The selected member may be a field or a define that is not shown
                // In that case try to select the class
                prot = _getUniqueName((XSourceEntity)member.Parent);
                if (_membersDict.TryGetValue(prot, out var parentLoc))
                {
                    return parentLoc;
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
                        // we only check the dates for the other files
                        if (String.Compare(file, _file.FullPath, StringComparison.OrdinalIgnoreCase) != 0)
                        {
                            var dt = File.GetLastWriteTime(file);
                            if (dt > _lastFileChanged)
                            {
                                _lastFileChanged = dt;
                                return true;
                            }
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
        /// <summary>
        /// Select type or Namespace
        /// </summary>
        /// <param name="newLine">Zero based line number</param>
        private void SelectContainingMember(int newLine)
        {
            if (_file == null)
                return;
            XSourceEntity selectedElement = _file.FindMemberAtRow(newLine);
            if (selectedElement == null)
                return;
            // check if we are before the first element (in the using area)
            if (selectedElement.Range.StartLine > newLine ||
                selectedElement.Range.EndLine < newLine)
            {
                selectedElement = _file.GlobalType;
            }
            XSourceTypeSymbol parentType = _file.GlobalType;
            if (selectedElement is XSourceMemberSymbol)
            {
                // For local functions the parentmember name is stored as Namespace
                // and the type is the parent of the member
                if (selectedElement.Kind.IsLocal())
                {
                    foreach (var member in _file.EntityList)
                    {
                        if (member.FullName == selectedElement.Namespace)
                        {
                            var parMember = member;
                            parentType = parMember.Parent as XSourceTypeSymbol;
                            break;
                        }
                    }

                }
                else
                {
                    parentType = (XSourceTypeSymbol)selectedElement.Parent;
                }
            }
            else if (selectedElement is XSourceTypeSymbol)
            {
                parentType = selectedElement as XSourceTypeSymbol;
            }
            bool newType = true;
            if (parentType != null && _lastType != null && parentType.FullName == _lastType.FullName)
            {
                newType = false;
            }
            var nothingChanged = _file.ContentHashCode == this._lastHashCode && !_settings.Changed && !relatedFilesChanged;
            // When the file has not changed and external files also have no changed
            // and when we are on the same type
            // then simply select another member
            if (nothingChanged)
            {
                _selectedTypeIndex = findSelectedType(parentType);
                if (newType && _settings.CurrentTypeOnly)
                {
                    // new Type and we only show members from current type
                    loadMemberCombos(_selectedTypeIndex);

                }
                _selectedMemberIndex = findSelectedMember(selectedElement);
                _saveSettings();
                refreshCombos();
            }
            else
            {
                reloadCombos(newLine);
                _selectedMemberIndex = findSelectedMember(selectedElement);
                _selectedTypeIndex = findSelectedType(parentType);
                refreshCombos();
            }
            _lastHashCode = _file.ContentHashCode;
#if XDEBUG
            _lastSelected = selectedElement;
#endif
            _lastType = parentType;
            return;
        }

        private IList<XSourceEntity> GetTypeMembers(XSourceTypeSymbol type)
        {
            var members = new List<XSourceEntity>();
            if (type.IsPartial && !type.IsGlobal && !_settings.ExcludeOtherFiles)
            {
                // Load members from all partial type definitions inside this project
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
                // Load the members from the type itself
                // but exclude the local functions and procedures
                if (type.IsGlobal)
                {
                    members.AddRange(type.XMembers.Where(e => !e.Kind.IsLocal()));
                }
                else
                {
                    members.AddRange(type.XMembers);
                }
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
            var includeFields = _settings.IncludeFields;
            members.AddRange(_file.EntityList.Where(member => includeFields || !member.Kind.IsField()));
            _lastFileChanged = DateTime.MinValue;
            // use ToArray to make sure that we will not suffer when the list gets changed
            foreach (var ent in _file.EntityList.ToArray())
            {
                if (ent is XSourceTypeSymbol)
                {
                    var xType = ent as XSourceTypeSymbol;
                    if (xType.IsPartial && !_settings.ExcludeOtherFiles)
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
            var sortItems = _settings.Sorted;
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
                //if (eltType.Kind == Kind.Namespace)
                //    continue;

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
            return;
        }

        private string _getUniqueName(XSourceEntity entity)
        {
            if (entity is XSourceMemberSymbol member)
                return member.ParentName + member.ComboPrototype;
            if (entity is XSourceTypeSymbol type)
                return type.FullName;
            if (entity != null)
                return entity.ToString();
            return "";
        }
        private void _addToDict(XSourceEntity entity)
        {
            _membersDict[_getUniqueName(entity)] = _members.Count - 1;
        }

        private void loadMemberCombos(int selectedType)
        {
            if (selectedType >= _types.Count)
                return;
            var currentType = (XSourceTypeSymbol)(selectedType > -1 ? _types[selectedType].Entity : _types[0].Entity);
            var globalType = _file.GlobalType;
            DROPDOWNFONTATTR ft;
            bool hasPartial = !currentType.IsGlobal && currentType.IsPartial;
            _members.Clear();
            _membersDict.Clear();
            _relatedFiles.Clear();
            if (!_settings.ExcludeOtherFiles)
            {
                AddSourceFile(_file.SourcePath);
            }
            var members = new List<XSourceEntity>();
            if (_settings.CurrentTypeOnly)
            {
                members.AddRange(GetTypeMembers(currentType));
                // also add local functions and procedures
                foreach (var member in _file.EntityList.Where( m => m.Kind.IsLocal()))
                {
                    if (member.Namespace.StartsWith(currentType.FullName))
                    {
                        members.Add(member);
                    }
                }
            }
            else
            {
                // get members of types in this file and members of partial types in other files.
                members.AddRange(GetAllMembers());
            }
            if (_settings.Sorted)
            {
                members = members.OrderBy(x => x.FullName).ToList();
            }
            // now either load all members or just the members of the current type
            // Add member for class declaration. This also makes sure that there at least one
            // element in the members list, which is convenient.
            TextSpan spM = this.TextRangeToTextSpan(currentType.Range);
            ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
            XDropDownMember elt;
            if (_settings.CurrentTypeOnly)
            {
                // Add a 'root' element for the type.
                if (currentType != globalType)
                {
                    if (currentType.Kind != Kind.Delegate)
                    {
                        elt = new XDropDownMember("(" + currentType.Name + ")", spM, currentType.Glyph, ft, currentType);
                        _members.Add(elt);
                        _addToDict(currentType);
                    }
                }
                else
                {
                    elt = new XDropDownMember(currentType.Name, spM, currentType.Glyph, ft, currentType);
                    _members.Add(elt);
                    _addToDict(currentType);
                }
            }
            else if (!_settings.IncludeFields)
            {
                elt = new XDropDownMember(globalType.Name, spM, globalType.Glyph, ft, globalType);
                _members.Add(elt);
                _addToDict(globalType);
            }
            foreach (XSourceEntity member in members)
            {
                bool otherFile;
                if (_settings.IncludeFields || (member.Kind != Kind.Field && member.Kind != Kind.VODefine))
                {
                    spM = this.TextRangeToTextSpan(member.Range);
                    otherFile = false;
                    ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                    if (member.Parent is XSourceTypeSymbol typedef && typedef.IsPartial)
                    {
                        otherFile = string.Compare(member.File.FullPath, _file.FullPath, true) != 0;
                    }
                    if (otherFile && _settings.ExcludeOtherFiles)
                    {
                        continue;
                    }
                    string prototype = member.ComboPrototype;
                    bool addPrefix = false;
                    if (_settings.CurrentTypeOnly)
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
                        if (_settings.CurrentTypeOnly)
                        {
                            var prefix = member.Namespace;
                            prefix = prefix.Substring(prefix.LastIndexOf('.')+1);
                            prototype = prefix + "." + prototype;
                        }
                        else
                        {
                            prototype = member.Namespace + "." + prototype;
                        }

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
                    _addToDict(member);
                }
            }
        }
        private void refreshCombos()
        {
            // Make sure the UI is updated on the foreground thread
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                if (_dropDownBar != null)
                {
                    if (_selectedProjectIndex != -1)
                        _dropDownBar.RefreshCombo(PROJECTINDEX, _selectedProjectIndex);
                    if (_selectedTypeIndex != -1)
                        _dropDownBar.RefreshCombo(TYPEINDEX, _selectedTypeIndex);
                    if (_selectedMemberIndex != -1)
                        _dropDownBar.RefreshCombo(MEMBERINDEX, _selectedMemberIndex);
                }
            });
        }

        private void reloadCombos(int nLine)
        {
            int temp = _selectedTypeIndex;
            loadTypeCombos(nLine);
            _selectedTypeIndex = temp;
            temp = _selectedMemberIndex;
            loadMemberCombos(_selectedTypeIndex);
            _selectedMemberIndex = temp;
            _lastHashCode = _file.ContentHashCode;
            _saveSettings();
            refreshCombos();
        }

        private TextSpan TextRangeToTextSpan(TextRange tr)
        {
            // It seems that TextSpan is Zero-based
            // and our TextRange is also Zero based
            // --> Make the move
            TextSpan ts = new TextSpan();
            ts.iStartLine = tr.StartLine;
            ts.iStartIndex = tr.StartColumn;
            ts.iEndLine = tr.EndLine;
            ts.iEndIndex = tr.EndColumn;
            // validate values
            if (ts.iStartLine < 0) ts.iStartLine = 0;
            if (ts.iStartIndex < 0) ts.iStartIndex = 0;
            if (ts.iEndLine < ts.iStartLine) ts.iEndLine = ts.iStartLine;
            if (ts.iEndIndex < ts.iStartIndex && ts.iStartLine == ts.iEndLine)
                ts.iEndIndex = ts.iStartIndex;
            return ts;
        }
        private void _saveSettings()
        {
            _settings = new DropdownSettings();
        }

        public async System.Threading.Tasks.Task RefreshDropDownAsync(bool needsUI)
        {
            try
            {
                if (_dropDownBar != null && _file != null)
                {
                    if (_file.ContentHashCode != _lastHashCode || _settings.Changed)
                    {
                        reloadCombos(_lastLine);
                    }
                    if (_activeView != null)
                    {
                        int caretPosition = _activeView.Caret.Position.BufferPosition.GetContainingLine().LineNumber;
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
                case TYPEINDEX: // types
                    entries = (uint)_types.Count;
                    break;
                case MEMBERINDEX: // members
                    entries = (uint)_members.Count;
                    break;
                case PROJECTINDEX: // projects
                    entries = (uint)_projects.Count; ;
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
            {
                return VSConstants.E_FAIL;
            }
            switch (combo)
            {
                case TYPEINDEX:
                    if (index < _types.Count)
                        attr = (uint)_types[index].FontAttr;
                    break;
                case MEMBERINDEX:
                    if (index < _members.Count)
                        attr = (uint)_members[index].FontAttr;
                    break;
                case PROJECTINDEX: // projects
                    attr = 0;
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
            {
                return VSConstants.E_FAIL;
            }
            switch (combo)
            {
                case TYPEINDEX: // types
                    if (index < _types.Count)
                        imageIndex = _types[index].Glyph;
                    break;
                case MEMBERINDEX: // members
                    if (index < _members.Count)
                        imageIndex = _members[index].Glyph;
                    break;
                case PROJECTINDEX: // projects
                    imageIndex = projectIcon; 
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
            {
                return VSConstants.E_FAIL;
            }
            switch (combo)
            {
                case TYPEINDEX: // types
                    if (index < _types.Count)
                        text = _types[index].Label;
                    break;
                case MEMBERINDEX: // members
                    if (index < _members.Count)
                        text = _members[index].Label;
                    break;
                case PROJECTINDEX: // projects
                    if (index < _projects.Count)
                        text = _projects[index].DisplayName;
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
                case TYPEINDEX:
                    if (index < _types.Count)
                        entity = _types[index].Entity;
                    break;
                case MEMBERINDEX:
                    if (index < _members.Count)
                        entity = _members[index].Entity;
                    break;
                case PROJECTINDEX: // projects
                    // process project selection
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
                if (!open)
                {
                    entity.OpenEditor();
                }
            }
            return VSConstants.S_OK;
        }

        private async Task<IVsTextView> GetActiveTextViewAsync()
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            IVsTextManager textManager = await ServiceProvider.GetGlobalServiceAsync<SVsTextManager, IVsTextManager>();
            ErrorHandler.ThrowOnFailure(textManager.GetActiveView(1, null, out IVsTextView activeView));
            return activeView;
        }

        public int SetDropdownBar(IVsDropdownBar dropdownBar)
        {
            if (dropdownBar != null)
            {
                _dropDownBar = dropdownBar;
                ThreadHelper.JoinableTaskFactory.Run(async () =>
                {
                    await RefreshDropDownAsync(needsUI: false);
                });
                if (_dropDownBar != null)
                {
                    refreshCombos();
                }
            }
            return VSConstants.S_OK;
        }

        public int OnItemSelected(int iCombo, int iIndex)
        {
            if (iCombo == TYPEINDEX)
                _selectedTypeIndex = iIndex;
            if (iCombo == MEMBERINDEX)
                _selectedMemberIndex = iIndex;
            if (iCombo == PROJECTINDEX)
            {
                _selectedProjectIndex = iIndex;
                _file.Project = ActiveProject;
                var classifier = _document.Buffer.GetClassifier();
                if (classifier != null)
                {
                    _ = classifier.ForceClassifyAsync();
                    classifier.TriggerRepaint(classifier.Snapshot);
                }

            }
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
