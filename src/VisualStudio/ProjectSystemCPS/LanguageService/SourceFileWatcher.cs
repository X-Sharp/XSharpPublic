//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using XSharp.Settings;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Reports source files of an X# CPS project that changed on disk, so the adapter can walk them again.
    /// </summary>
    /// <remarks>
    /// MPFproj does this in XSharpProjectNode.OnFileChanged (file change notifications for its items, then
    /// WalkFile(file, notify: true)). Under CPS nobody re-read a file that changed on disk: the VO designers write the
    /// generated .prg directly to disk when it is not open in an editor, and the code model kept the old types until
    /// the next project walk (verified in VS). The same applies to saves from the VS editor, external editors and
    /// source control operations, which this watcher covers as well.
    /// A FileSystemWatcher on the project folder (including subfolders); the adapter decides which paths are source
    /// files of the project. Changes are collected and reported after <see cref="Delay"/>, so a burst of writes
    /// (a designer save writes several files, editors write via temporary files) gives one walk per file.
    /// Files outside the project folder (links) are not watched.
    /// </remarks>
    internal sealed class SourceFileWatcher : IDisposable
    {
        private static readonly TimeSpan Delay = TimeSpan.FromMilliseconds(500);

        private readonly Func<string, bool> isSourceFile;
        private readonly Action<string> fileChanged;
        private readonly Action changesLost;
        private readonly FileSystemWatcher watcher;
        private readonly Timer timer;
        private readonly object gate = new object();
        private readonly HashSet<string> pending = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        private bool disposed;

        /// <param name="folder">The project folder.</param>
        /// <param name="isSourceFile">True for the full path of a source file of the project (called on the watcher's thread).</param>
        /// <param name="fileChanged">Called with the full path of each changed source file, on a background thread.</param>
        /// <param name="changesLost">Called when changes may have been missed (the watcher's buffer overflowed).</param>
        public SourceFileWatcher(string folder, Func<string, bool> isSourceFile, Action<string> fileChanged, Action changesLost)
        {
            this.isSourceFile = isSourceFile;
            this.fileChanged = fileChanged;
            this.changesLost = changesLost;
            timer = new Timer(OnTimer, null, Timeout.Infinite, Timeout.Infinite);
            watcher = new FileSystemWatcher(folder)
            {
                IncludeSubdirectories = true,
                NotifyFilter = NotifyFilters.FileName | NotifyFilters.LastWrite | NotifyFilters.Size,
                InternalBufferSize = 64 * 1024,
            };
            watcher.Changed += (sender, e) => Queue(e.FullPath);
            watcher.Created += (sender, e) => Queue(e.FullPath);
            watcher.Renamed += (sender, e) => Queue(e.FullPath);
            watcher.Error += OnError;
            watcher.EnableRaisingEvents = true;
        }

        private void Queue(string path)
        {
            try
            {
                if (string.IsNullOrEmpty(path) || !isSourceFile(path))
                    return;
                lock (gate)
                {
                    if (disposed)
                        return;
                    pending.Add(path);
                    timer.Change(Delay, Timeout.InfiniteTimeSpan);
                }
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
        }

        private void OnTimer(object state)
        {
            string[] files;
            lock (gate)
            {
                if (disposed)
                    return;
                files = pending.ToArray();
                pending.Clear();
            }
            foreach (var file in files)
            {
                try
                {
                    fileChanged(file);
                }
                catch (Exception e)
                {
                    XSettings.Exception(e);
                }
            }
        }

        private void OnError(object sender, ErrorEventArgs e)
        {
            XSettings.Information("SourceFileWatcher: " + e.GetException()?.Message + ", walking the project again");
            try
            {
                changesLost();
            }
            catch (Exception ex)
            {
                XSettings.Exception(ex);
            }
        }

        public void Dispose()
        {
            lock (gate)
            {
                if (disposed)
                    return;
                disposed = true;
                pending.Clear();
            }
            watcher.EnableRaisingEvents = false;
            watcher.Dispose();
            timer.Dispose();
        }
    }
}
