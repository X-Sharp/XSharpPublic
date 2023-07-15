using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Serilog;
using System.IO;
using XSharpModel;
using XSharp.Settings;
using System.ComponentModel;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System.Diagnostics;

namespace XSharp.Project
{
    internal static class Logger
    {
        static bool log2debugger = false;
        static bool log2file = false;

        static Logger()
        {
            XSettings.Logger = new LoggerImpl();
        }

        private static string singleline = new string('-', 80);
        private static string doubleline = new string('=', 80);

        static bool active = false;
        internal static bool Active => active;
        internal static void Start()
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
#pragma warning disable CS0618 // Type or member is obsolete
                    int threadid = AppDomain.GetCurrentThreadId();
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
                var sol = VS.Solutions.GetCurrentSolution();
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


        internal static void Stop()
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

        internal static void Debug(string message)
        {
            if (active)
            {
                Log.Debug(formatMessage(message));
            }

        }
        internal static void Information(string message)
        {
            if (active)
            {
                Log.Information(formatMessage(message));
            }
        }


        internal static void Exception(Exception e, string message)
        {
            if (active)
            {
                Log.Error(e, formatMessage(message));
            }
        }
        internal static void SingleLine()
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
