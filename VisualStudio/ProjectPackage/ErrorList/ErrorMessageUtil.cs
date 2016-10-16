using Microsoft.VisualStudio.Shell.Interop;


namespace TaskOutputListener
{
    public enum MessageSeverity
    {
        Info = 0,
        Warning = 1,
        Error = 2,
        NoProblems = 3
    }

    internal static class ErrorMessageUtil
    {
        static public __VSERRORCATEGORY ToVSERRORCATEGORY(MessageSeverity severity)
        {
            if (severity == MessageSeverity.Warning)
            {
                return __VSERRORCATEGORY.EC_WARNING;
            }
            else if (severity == MessageSeverity.Error)
            {
                return __VSERRORCATEGORY.EC_ERROR;
            }
            return __VSERRORCATEGORY.EC_MESSAGE;
        }
    }


}
