//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using EnvDTE;
using System;
using System.Diagnostics;

namespace XSharp.Project.FileCodeModel
{
    public class CodeDomEditPoint : EditPoint
    {
        private EditPoint _parent;

        public CodeDomEditPoint( EditPoint parent )
        {
            _parent = parent;
        }

        public bool EqualTo(TextPoint Point)
        {
            return _parent.EqualTo(Point);
        }

        public bool LessThan(TextPoint Point)
        {
            return _parent.LessThan(Point);
        }

        public bool GreaterThan(TextPoint Point)
        {
            return _parent.GreaterThan(Point);
        }

        public bool TryToShow(vsPaneShowHow How = vsPaneShowHow.vsPaneShowCentered, object PointOrCount = null)
        {
            return _parent.TryToShow(How, PointOrCount);
        }

        public EditPoint CreateEditPoint()
        {
            return _parent.CreateEditPoint();
        }

        public void CharLeft(int Count = 1)
        {
            _parent.CharLeft(Count);
        }

        public void CharRight(int Count = 1)
        {
            _parent.CharRight(Count);
        }

        public void EndOfLine()
        {
            _parent.EndOfLine();
        }

        public void StartOfLine()
        {
            _parent.StartOfLine();
        }

        public void EndOfDocument()
        {
            _parent.EndOfDocument();
        }

        public void StartOfDocument()
        {
            _parent.StartOfDocument();
        }

        public void WordLeft(int Count = 1)
        {
            _parent.WordLeft(Count);
        }

        public void WordRight(int Count = 1)
        {
            _parent.WordRight(Count);
        }

        public void LineUp(int Count = 1)
        {
            _parent.LineUp(Count);
        }

        public void LineDown(int Count = 1)
        {
            _parent.LineDown(Count);
            
        }

        public void MoveToPoint(TextPoint Point)
        {
            _parent.MoveToPoint(Point);
            
        }

        public void MoveToLineAndOffset(int Line, int Offset)
        {
            _parent.MoveToLineAndOffset(Line, Offset);
            
        }

        public void MoveToAbsoluteOffset(int Offset)
        {
            _parent.MoveToAbsoluteOffset(Offset);
            
        }

        public void SetBookmark()
        {
            _parent.SetBookmark();
            
        }

        public void ClearBookmark()
        {
            _parent.ClearBookmark();
            
        }

        public bool NextBookmark()
        {
            return _parent.NextBookmark();
        }

        public bool PreviousBookmark()
        {
            return _parent.PreviousBookmark();
        }

        public void PadToColumn(int Column)
        {
            _parent.PadToColumn(Column);
        }

        public void Insert(string Text)
        {
            _parent.Insert(Text);
        }

        public void InsertFromFile(string File)
        {
            _parent.InsertFromFile(File);
        }

        public string GetText(object PointOrCount)
        {
            return _parent.GetText(PointOrCount);
        }

        public string GetLines(int Start, int ExclusiveEnd)
        {
            return _parent.GetLines(Start, ExclusiveEnd);   
        }

        public void Copy(object PointOrCount, bool Append = false)
        {
            _parent.Copy(PointOrCount, Append);
        }

        public void Cut(object PointOrCount, bool Append = false)
        {
            _parent.Cut(PointOrCount, Append);  
        }

        public void Delete(object PointOrCount)
        {
            _parent.Delete(PointOrCount);
        }

        public void Paste()
        {
            _parent.Paste();
        }

        public bool ReadOnly(object PointOrCount)
        {
            return _parent.ReadOnly(PointOrCount);
        }

        public bool FindPattern(string Pattern, int vsFindOptionsValue , ref EditPoint EndPoint, ref TextRanges Tags)
        {
            return _parent.FindPattern(Pattern, vsFindOptionsValue, ref EndPoint,ref Tags);
        }

        public bool ReplacePattern(TextPoint Point, string Pattern, string Replace, int vsFindOptionsValue, ref TextRanges Tags)
        {
            return _parent.ReplacePattern( Point, Pattern, Replace, vsFindOptionsValue,ref Tags);
        }

        public void Indent(TextPoint Point = null, int Count = 1)
        {
            _parent.Indent(Point, Count);
        }

        public void Unindent(TextPoint Point = null, int Count = 1)
        {
            _parent.Unindent(Point, Count); 
        }

        public void SmartFormat(TextPoint Point)
        {
            _parent.SmartFormat(Point);
        }

        public void OutlineSection(object PointOrCount)
        {
            _parent.OutlineSection(PointOrCount);
        }

        public void ReplaceText(object PointOrCount, string Text, int Flags)
        {
            try
            {
                _parent.ReplaceText(PointOrCount, Text, Flags);
            }
            catch (Exception e)
            {
                Debug.WriteLine( "EditPoint.Message : " + e.Message );
            }
        }

        public void ChangeCase(object PointOrCount, vsCaseOptions How)
        {
            _parent.ChangeCase(PointOrCount, How);
        }

        public void DeleteWhitespace(vsWhitespaceOptions Direction = vsWhitespaceOptions.vsWhitespaceOptionsHorizontal)
        {
            _parent.DeleteWhitespace(Direction);
        }

        public DTE DTE => _parent.DTE;

        public TextDocument Parent => _parent.Parent;

        public int Line => _parent.Line;

        public int LineCharOffset => _parent.LineCharOffset;

        public int AbsoluteCharOffset => _parent.AbsoluteCharOffset;

        public int DisplayColumn => _parent.DisplayColumn;

        public bool AtEndOfDocument => _parent.AtEndOfDocument;

        public bool AtStartOfDocument => _parent.AtStartOfDocument;

        public bool AtEndOfLine => _parent.AtEndOfLine;

        public bool AtStartOfLine => _parent.AtStartOfLine;

        public int LineLength => _parent.LineLength;

        public CodeElement get_CodeElement(vsCMElement Scope)
        {
            return _parent.get_CodeElement(Scope);  
        }

    }
}
