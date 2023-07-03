using System;
using System.Collections.Generic;
using Serilog;
using System.IO;
using XSharpModel;
using System.ComponentModel;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System.Diagnostics;

namespace XSharp.LanguageService
{
    internal class Logger : XSharpModel.ILogger
    {
        bool log2debugger = false;
        bool log2file = false;
        static Logger instance;
        private static string singleline = new string('-', 80);
        private static string doubleline = new string('=', 80);

        static bool active = false;
        public bool Active => active;

        internal static void InitializeLogger()
        {
            instance = new Logger();
            XSolution.Logger = instance;
        }
        internal static void ActivateWhenNeeded()
        {
            bool wasActive = active;
            int FileLogging = (int)Constants.GetSetting("Log2File", 0);
            int DebugLogging = (int)Constants.GetSetting("Log2Debug", 0);


            XSettings.EnableFileLogging = FileLogging != 0;
            XSettings.EnableDebugLogging = DebugLogging != 0;
            var newActive = XSettings.EnableFileLogging || XSettings.EnableDebugLogging;
            if (newActive != wasActive)
            {
                if (newActive)
                    instance.Start();
                else
                    instance.Stop();
                active = newActive;
            }
            

        }
        public void Start()
        {
            if (!active ||
                log2debugger != XSettings.EnableDebugLogging ||
                log2file != XSettings.EnableFileLogging)
            {
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
                    int threadid = 0;
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        threadid = AppDomain.GetCurrentThreadId();
                    });
#pragma warning disable CS0618 // Type or member is obsolete
                    
#pragma warning restore CS0618 // Type or member is obsolete
                    string strId = threadid.ToString("X");
                    var log = Path.Combine(temp, "Project_" + strId + "_.log");
                    config = config.WriteTo.File(log,
                        rollingInterval: RollingInterval.Day,
                        rollOnFileSizeLimit: true,
                        flushToDiskInterval: TimeSpan.FromSeconds(15),
                        retainedFileCountLimit: 5);
                    log2file = true;
                }
                Log.CloseAndFlush();
                Log.Logger = config.CreateLogger();

                Log.Information(doubleline);
                Log.Information("Started Logging");
                string version = "";
                bool isOpening = false;     // This is TRUE when we are opening VS with a solution from the commandline
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    var ver = await VS.Shell.GetVsVersionAsync();
                    isOpening = await VS.Solutions.IsOpeningAsync();
                    version = ver.ToString();
                });
                Log.Information("Visual Studio Exe     : " + Process.GetCurrentProcess().MainModule.FileName);
                Log.Information("Visual Studio version : " + version);
                Log.Information("XSharp Project System : " + Constants.FileVersion);
                Log.Information("Commandline           : " + Environment.CommandLine.ToString());


                Log.Information(doubleline);
                Solution sol = null;
                try
                {
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        sol = await VS.Solutions.GetCurrentSolutionAsync();
                    });
                }
                catch (Exception )
                {
                    sol = null;
                }
                if (sol != null )
                {
                    Log.Information(singleline);
                    Log.Information("Current solution: " + sol.FullPath);
                    // we only want to enum projects when the solution explorer window is already visible
                    bool enumProjects = ! isOpening;
                    if ( enumProjects)
                    {
                        ThreadHelper.JoinableTaskFactory.Run(async delegate
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

                        });
                    }
                    if (enumProjects)
                    {
                        var children = EnumChildren(sol, SolutionItemType.Project);
                        try
                        {
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
                    active = true;
                }
            }
            // Force all Logging options to be enabled
            XSettings.EnableAll();
        }

         IList<SolutionItem> EnumChildren(SolutionItem item, SolutionItemType type)
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

        private void CurrentDomain_FirstChanceException(object sender, System.Runtime.ExceptionServices.FirstChanceExceptionEventArgs e)
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

        private void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            if (active)
            {
                if (e.ExceptionObject is Exception ex)
                    Log.Fatal(ex, formatMessage("UnhandledException"));
            }
        }


        public void Stop()
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

        public void Debug(string message)
        {
            if (active)
            {
                Log.Debug(formatMessage(message));
            }

        }
        public void Information(string message)
        {
            if (active)
            {
                Log.Information(formatMessage(message));
            }
        }


        public void Exception(Exception e, string message)
        {
            if (active)
            {
                Log.Error(e, formatMessage(message));
            }
        }
        public void SingleLine()
        {
            Information(singleline);
        }
        public void DoubleLine()
        {
            Information(doubleline);
        }

        private static string formatMessage(string message)
        {
            return string.Format("{0:X4} {1}", System.Threading.Thread.CurrentThread.ManagedThreadId, message);
        }

    }
}
