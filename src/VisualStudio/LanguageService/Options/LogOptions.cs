//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharp.Settings;

namespace XSharp.LanguageService
{


    public class LogOptions : OptionsBase
    {
        internal static LogOptions Instance;
        static LogOptions()
        {
            Instance = new LogOptions();
        }
        private LogOptions()
        {
            Instance = this;
            ReadFromRegistry();
        }
        public bool LogToFile { get; set; }
        public bool LogToDebug { get; set; }

        public override void WriteToSettings()
        {
            XSettings.EnableFileLogging = LogToFile;
            XSettings.EnableDebugLogging = LogToDebug;
            Constants.WriteSetting("Log2File", LogToFile ? 1 : 0);
            Constants.WriteSetting("Log2Debug", LogToDebug ? 1 : 0);
        }
        internal void ReadFromRegistry()
        {
            LogToFile = (int)Constants.GetSetting("Log2File", 0) != 0;
            LogToDebug = (int)Constants.GetSetting("Log2Debug", 0) != 0;

        }

    }
}
