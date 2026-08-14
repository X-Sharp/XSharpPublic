using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Shell;

using Serilog;

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;


using XSharp.Settings;

using XSharpModel;

namespace XSharp.Support
{
    public static class Logger
    {
        static bool log2debugger = false;
        static bool log2file = false;
        static object gate = new object ();
        static bool initialized = false;

        /// <summary>
        /// Initialize the logger and the link to the VS shell.
        /// </summary>
        /// <remarks>
        /// Nothing on this path may block the calling thread, because this is called from the
        /// InitializeAsync() of our packages. The shell can load those packages synchronously from
        /// Solution.OpenAsync() -> CanOpenProject() when a solution is opened while VS is starting
        /// (a pinned solution or a solution on the commandline). The UI thread then waits for the
        /// package load to complete, so anything here that blocks while waiting for the UI thread -
        /// a JoinableTaskFactory.Run() or one of the synchronous Community Toolkit wrappers -
        /// deadlocks the IDE. The work that needs the UI thread is therefore awaited and never
        /// waited on: see StartAsync(), LogEnvironmentAsync() and XSharpShellLink.InitializeAsync().
        /// Flags are still set inside the lock and everything else happens after it is released,
        /// so we also never hold the lock while waiting for another thread.
        /// </remarks>
        public static async System.Threading.Tasks.Task<bool> InitializeAsync()
        {
            bool needsShellLink = false;
            bool shouldStart = false;
            bool alreadyActive;

            lock (gate)
            {
                // Initialize XSettings.ShellLink and Logger only once, before accessing Logger methods
                if (!initialized)
                {
                    XSettings.Logger = new LoggerImpl();
                    initialized = true;
                    needsShellLink = true;
                }

                alreadyActive = active;
                if (!active)
                {
                    int fileLogging = (int)Constants.GetSetting("Log2File", XSettings.EnableFileLogging ? 1 : 0);
                    int debugLogging = (int)Constants.GetSetting("Log2Debug", XSettings.EnableDebugLogging ? 1 : 0);

                    XSettings.EnableFileLogging = fileLogging != 0;
                    XSettings.EnableDebugLogging = debugLogging != 0;
                    shouldStart = XSettings.EnableFileLogging || XSettings.EnableDebugLogging;
                }
            }
            if (needsShellLink)
            {
                var shellLink = new XSharpShellLink();
                XSettings.ShellLink = shellLink;
                await shellLink.InitializeAsync();
            }
            if (alreadyActive)
                return active;
            if (shouldStart)
                await Logger.StartAsync();
            else
                Logger.Stop();
            return shouldStart;
        }


        private static readonly string singleline = new string('-', 80);
        private static readonly string doubleline = new string('=', 80);

        static bool active = false;
        internal static bool Active => active;
        /// <summary>
        /// Start logging. The environment is logged in the background, because that part needs the
        /// UI thread. Callers that can await should use StartAsync().
        /// </summary>
        public static void Start()
        {
            try
            {
                if (ConfigureSerilog())
                {
                    active = true;
                    LogEnvironmentAsync().FireAndForget();
                }
                // Force all Logging options to be enabled
                XSettings.EnableAll();
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }

        /// <summary>
        /// Start logging without ever blocking the calling thread. Use this from package
        /// initialization. See the remarks on InitializeAsync().
        /// </summary>
        public static async System.Threading.Tasks.Task StartAsync()
        {
            try
            {
                if (ConfigureSerilog())
                {
                    active = true;
                    await LogEnvironmentAsync();
                }
                // Force all Logging options to be enabled
                XSettings.EnableAll();
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }

        /// <summary>
        /// (Re)create the Serilog logger. This is pure configuration: no VS services, no UI thread.
        /// </summary>
        /// <returns>TRUE when the logger was (re)created, so the environment must be logged again</returns>
        private static bool ConfigureSerilog()
        {
            if (active &&
                log2debugger == XSettings.EnableDebugLogging &&
                log2file == XSettings.EnableFileLogging)
            {
                return false;
            }
            if (active)
            {
                Stop();
            }
            var config = new LoggerConfiguration()
                    .MinimumLevel.Debug();
            log2debugger = false;
            log2file = false;
            if (XSettings.EnableDebugLogging)
            {
                config = config.WriteTo.Debug();
                log2debugger = true;
            }
            if (XSettings.EnableFileLogging)
            {
                var temp = Path.GetTempPath();
                temp = Path.Combine(temp, "XSharp.Intellisense");
                if (!Directory.Exists(temp))
                {
                    Directory.CreateDirectory(temp);
                }
                int threadid = Process.GetCurrentProcess().Id;
                string strId = threadid.ToString("X");
                var log = Path.Combine(temp, "Project_" + strId + "_.log");
                config = config.WriteTo.File(log,
                    rollingInterval: RollingInterval.Day,
                    rollOnFileSizeLimit: true,
                    flushToDiskInterval: TimeSpan.FromSeconds(15),
                    retainedFileCountLimit: 5);
                log2file = true;
            }


            Log.Logger = config.CreateLogger();

            Log.Information(doubleline);
            Log.Information("Started Logging");
            return true;
        }

        /// <summary>
        /// Log the environment and register the current solution with the code model.
        /// This needs the UI thread, so it must be awaited and never waited on.
        /// </summary>
        internal static async System.Threading.Tasks.Task LogEnvironmentAsync()
        {
            try
            {
                var ver = await VS.Shell.GetVsVersionAsync();
                // This is TRUE when we are opening VS with a solution from the commandline,
                // which is also what happens for a solution pinned to the start window or the jumplist
                bool isOpening = await VS.Solutions.IsOpeningAsync();
                Log.Information("Visual Studio Exe     : " + Process.GetCurrentProcess().MainModule.FileName);
                Log.Information("Visual Studio version : " + ver?.ToString());
                Log.Information("XSharp Project System : " + Constants.FileVersion);
                Log.Information("Commandline           : " + Environment.CommandLine.ToString());


                Log.Information(doubleline);
                var sol = await VS.Solutions.GetCurrentSolutionAsync();
                if (sol == null)
                {
                    return;
                }
                Log.Information(singleline);
                Log.Information("Current solution: " + sol.FullPath);
                if (!XSolution.IsOpen)
                    XSolution.Open(sol.FullPath);
                // we only want to enum projects when the solution explorer window is already visible
                bool enumProjects = !isOpening;
                if (enumProjects)
                {
                    try
                    {
                        var solwin = await VS.Windows.GetSolutionExplorerWindowAsync();
                        enumProjects = solwin != null;
                    }
                    catch (Exception)
                    {
                        // This happens when the solution explorer is not visible yet
                        // do not enum the projects then
                        enumProjects = false;
                    }
                }
                if (enumProjects)
                {
                    // walking the solution items is COM work, so we need the UI thread for that
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    try
                    {
                        var children = EnumChildren(sol, SolutionItemType.Project);
                        if (children != null)
                        {
                            foreach (var child in children)
                            {
                                if (child.Type == SolutionItemType.Project)
                                {
                                    Log.Information("Project " + child.FullPath);
                                }
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        Log.Error(e.Message);
                    }
                }
                else
                {
                    Log.Information("No projects opened yet");
                }
                Log.Information(singleline);

                AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
                //AppDomain.CurrentDomain.FirstChanceException += CurrentDomain_FirstChanceException;
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }

        static IList<SolutionItem> EnumChildren(SolutionItem item, SolutionItemType type)
        {
            var items = new List<SolutionItem>();
            foreach (var child in item.Children)
            {
                if (child != null && child.Type != SolutionItemType.Unknown)
                {
                    if (child.Type == type)
                    {
                        items.Add(child);
                    }
                    try
                    {
                        items.AddRange(EnumChildren(child, type));
                    }
                    catch (Exception e)
                    {
                        Exception(e, "EnumChildren");
                    }
                }
            }
            return items;
        }

        private static void CurrentDomain_FirstChanceException(object sender, System.Runtime.ExceptionServices.FirstChanceExceptionEventArgs e)
        {

            if (active)
            {
                if (e.Exception is OperationCanceledException)
                {
                    ; // do nothing
                }
                else if (e.Exception is Win32Exception)
                {
                    ; // do nothing
                }
                else
                {
                    Log.Error(e.Exception, formatMessage("FirstChanceException"));
                }
            }
        }

        private static void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            if (active)
            {
                if (e.ExceptionObject is Exception ex)
                    Log.Fatal(ex, formatMessage("UnhandledException"));
            }
        }


        public static void Stop()
        {
            if (active)
            {
                Log.Information(doubleline);
                Log.Information("Stopped logging");
                Log.Information(doubleline);

                Log.CloseAndFlush();
                active = false;
                AppDomain.CurrentDomain.UnhandledException -= CurrentDomain_UnhandledException;
                //AppDomain.CurrentDomain.FirstChanceException -= CurrentDomain_FirstChanceException;
            }
        }

        public static void Debug(string message)
        {
            if (active)
            {
                Log.Debug(formatMessage(message));
            }

        }
        public static void Information(string message)
        {
            if (active)
            {
                Log.Information(formatMessage(message));
            }
        }
        public static void Error(string message)
        {
            if (active)
            {
                Log.Error(formatMessage(message));
            }
        }


        public static void Exception(Exception e, string message)
        {
            if (active)
            {
                Log.Error(e, formatMessage(message));
            }
        }
        public static void SingleLine()
        {
            Information(singleline);
        }
        internal static void DoubleLine()
        {
            Information(doubleline);
        }

        private static string formatMessage(string message)
        {
            return string.Format("{0:X4} {1}", System.Threading.Thread.CurrentThread.ManagedThreadId, message);
        }

    }

    internal class LoggerImpl : XSharpModel.ILogger
    {
        public bool Active => Logger.Active;

        public void Debug(string sMsg)
        {
            Logger.Debug(sMsg);
        }

        public void DoubleLine()
        {
            Logger.DoubleLine();
        }

        public void Error(string sMsg)
        {
            Logger.Error(sMsg);
        }

        public void Exception(Exception e, string sMsg)
        {
            Logger.Debug(sMsg);
        }

        public void Information(string sMsg)
        {
            Logger.Information(sMsg);
        }

        public void SingleLine()
        {
            Logger.SingleLine();
        }

        public void Start()
        {
            Logger.Start();
        }

        public void Stop()
        {
            Logger.Stop();
        }

    }
}
