using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using System.Collections.Generic;


namespace TaskOutputListener
{
    internal class OutputErrorSnapshot : WpfTableEntriesSnapshotBase
    {
        private const int UndefinedLineOrColumn = -1;
        private readonly int _versionNumber;
        public readonly List<IErrorListItem> Errors;

        internal OutputErrorSnapshot(int versionNumber, IList<IErrorListItem> errors)
        {
            _versionNumber = versionNumber;
            Errors = new List<IErrorListItem>(errors);
        }

        public override int Count
        {
            get
            {
                return Errors.Count;
            }
        }

        public override int VersionNumber
        {
            get
            {
                return _versionNumber;
            }
        }

        /// <summary>
        /// Attempts to get a column value for the item at index in the snapshot. Note that this method
        /// gets called for any columns that the control supports. If we do not have a value
        /// to supply for the column or if somehow we got an invalid index the content needs to be
        /// set to null and return false which lets the table control set the value.
        /// </summary>
        public override bool TryGetValue(int index, string columnName, out object content)
        {
            if (index >= 0 && index < Errors.Count)
            {
                var error = Errors[index];
                switch (columnName)
                {
                    case StandardTableKeyNames.Text:
                        content = error.Message;
                        return true;
                    case StandardTableKeyNames.ProjectName:
                        content = error.ProjectName;
                        return true;
                    case StandardTableKeyNames.ProjectGuid:
                        content = "";
                        return true;
                    case StandardTableKeyNames.DocumentName:
                        content = error.Filename;
                        return true;
                    case StandardTableKeyNames.Line:
                        content = GetErrorListAdjustedLineOrColumn(error.Line);
                        return true;
                    case StandardTableKeyNames.Column:
                        content = GetErrorListAdjustedLineOrColumn(error.Column);
                        return true;
                    case StandardTableKeyNames.ErrorSeverity:
                        content = ErrorMessageUtil.ToVSERRORCATEGORY(error.Severity);
                        return true;
                    case StandardTableKeyNames.TaskCategory:
                        content = VSTASKCATEGORY.CAT_BUILDCOMPILE;
                        return true;
                    case StandardTableKeyNames.ErrorSource:
                        content = ErrorSource.Build;
                        return true;
                    case StandardTableKeyNames.ErrorCode:
                        content = error.ErrorCode;
                        return true;
                    case StandardTableKeyNames.BuildTool:
                        content = Vsix.Name;
                        return true;
                }
            }

            // This method gets called for anything that the control supports. If we do not have a value
            // to supply for the column or if somehow we got an invalid index the content needs to be
            // set to null and return false which lets the table control set the value.
            content = null;
            return false;
        }


        /// <summary>
        /// Helper to account for the fact the error list is 0 based.
        /// </summary>
        private static int GetErrorListAdjustedLineOrColumn(int lineOrColumn)
        {
            if (lineOrColumn != UndefinedLineOrColumn)
            {
                return lineOrColumn > 0 ? lineOrColumn - 1 : 0;
            }

            return lineOrColumn;
        }
    }
}