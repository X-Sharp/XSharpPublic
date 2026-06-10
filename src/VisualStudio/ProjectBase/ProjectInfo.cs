
using System.Diagnostics;
using System;
using System.Collections.Concurrent;

using Microsoft.VisualStudio.Shell.Interop;

namespace Microsoft.VisualStudio.Project
{
    /// <summary>
    /// This is a light weight class to link URL with Guid to keep track of dependencies between projects.
    /// </summary>
#if DEBUG
    [DebuggerDisplay("ProjectInfo: {Name}, {IdString}")]
#endif
    public class ProjectInfo
    {
        public string Url { get; private set; }
        public Guid Id { get; private set; }
        public IVsHierarchy Hierarchy { get; set; } = null;
#if DEBUG
        public string Name => System.IO.Path.GetFileNameWithoutExtension(Url);

        public string IdString => Id.ToString("B").ToUpper();
#endif
        private static ConcurrentDictionary<string, ProjectInfo> _projectsByUrl;
        private static ConcurrentDictionary<Guid, ProjectInfo> _projectsById;


        static ProjectInfo()
        {
            _projectsByUrl = new ConcurrentDictionary<string, ProjectInfo>(StringComparer.OrdinalIgnoreCase);
            _projectsById = new ConcurrentDictionary<Guid, ProjectInfo>();
        }

        public ProjectInfo(Guid id, string url)
        {
            Debug.Assert(id != Guid.Empty, "ProjectInfo should have a valid Guid");
            Logger.Information($"Creating new ProjectInfo for {url} with guid {id}");
            this.Id = id;
            this.Url = url;
            this.Hierarchy = null;
            _projectsByUrl[url] = this;
            _projectsById[id] = this;
        }

        public static ProjectInfo GetProjectInfo(Guid id)
        {
            _projectsById.TryGetValue(id, out var projectInfo);
            return projectInfo;
        }
        public static ProjectInfo GetProjectInfo(string url)
        {
            _projectsByUrl.TryGetValue(url, out var projectInfo);
            return projectInfo;
        }
        public static ProjectInfo GetProjectInfo(string url, Guid guid)
        {
            var result = GetProjectInfo(url);
            if (result == null && guid != Guid.Empty)
            {
                result = GetProjectInfo(guid);
            }
            return result;
        }

        public static void RemoveProjectInfo(string url, Guid id)
        {
            var projectInfo = GetProjectInfo(url, id);
            Remove(projectInfo);
        }
        public static void Remove(ProjectInfo projectInfo)
        {
            if (projectInfo != null)
            {
                Logger.Information($"Removing projectInfo for {projectInfo.Url} with guid {projectInfo.Id}");
                _projectsByUrl.TryRemove(projectInfo.Url, out _);
                if (projectInfo.Id != Guid.Empty)
                    _projectsById.TryRemove(projectInfo.Id, out _);
            }
        }
    }
}
