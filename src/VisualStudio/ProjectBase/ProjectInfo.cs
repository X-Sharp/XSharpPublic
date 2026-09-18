
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

        private IVsHierarchy _hierarchy = null;
        private EnvDTE.Project _dteProject = null;

        /// <summary>
        /// The hierarchy of this project, once somebody has resolved it.
        /// </summary>
        /// <remarks>
        /// Resolving a hierarchy from a guid enumerates the whole solution, so it is well
        /// worth caching. Setting this to null is how close, unload and reload invalidate
        /// the entry - see the ClearHierarchy methods.
        /// </remarks>
        public IVsHierarchy Hierarchy
        {
            get { return _hierarchy; }
            set
            {
                _hierarchy = value;
                if (value == null)
                {
                    // Invalidation: the automation object was resolved from the hierarchy
                    // we are dropping, so it has to go too. Replacing it with another non
                    // null hierarchy is NOT an invalidation: the shell can hand out a
                    // different runtime wrapper for the very same project, and treating
                    // that as a change threw the cache away on almost every write.
                    _dteProject = null;
                }
            }
        }

        /// <summary>
        /// The automation object of this project, resolved from <see cref="Hierarchy"/>.
        /// </summary>
        /// <remarks>
        /// Shared by every project reference that points at this project, so the resolution
        /// happens once per project instead of once per reference node - on a solution with
        /// 226 projects that is 88 resolutions instead of 3283. It is dropped whenever
        /// <see cref="Hierarchy"/> is cleared, so it cannot outlive the project it belongs to.
        /// </remarks>
        public EnvDTE.Project DteProject
        {
            get { return _hierarchy == null ? null : _dteProject; }
            set { _dteProject = value; }
        }
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

        /// <summary>
        /// Forget the cached hierarchy of every ProjectInfo that points to it.
        /// </summary>
        /// <remarks>
        /// Readers of <see cref="Hierarchy"/> take a non null value as proof that the project is
        /// still loaded, so it has to be dropped as soon as that project is closed, unloaded or
        /// reloaded. Removing the whole ProjectInfo only happens for our own project nodes
        /// (ProjectNode.Close()), so foreign projects need this. Clearing too eagerly costs
        /// nothing: the next reader resolves the hierarchy through the shell and caches it again.
        /// </remarks>
        public static void ClearHierarchy(IVsHierarchy hierarchy)
        {
            if (hierarchy == null)
            {
                return;
            }
            foreach (var projectInfo in _projectsByUrl.Values)
            {
                if (ReferenceEquals(projectInfo.Hierarchy, hierarchy))
                {
                    Logger.Information($"Dropping cached hierarchy for {projectInfo.Url} with guid {projectInfo.Id}");
                    projectInfo.Hierarchy = null;
                }
            }
        }

        /// <summary>
        /// Forget all cached hierarchies, for when the whole solution goes away.
        /// </summary>
        public static void ClearHierarchies()
        {
            Logger.Information("Dropping all cached project hierarchies");
            foreach (var projectInfo in _projectsByUrl.Values)
            {
                projectInfo.Hierarchy = null;
            }
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
