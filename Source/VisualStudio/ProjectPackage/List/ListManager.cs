using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    internal class ListManager<T> where T:IListItem
    {
        protected XSharpProjectNode Project { get; set; }
        protected ListFactory<T> Factory { get; set; }
        protected IList<T> Items { get; set; }
        protected bool dirty;
        internal ListManager(XSharpProjectNode node, ListProvider provider)
        {
            Project = node;
            Items = new List<T>();
            Factory = new ListFactory<T>(provider, node.ProjectIDGuid);
        }

        internal void Clear()
        {
            lock (this)
            {
                // Replace collection to prevent MT errors
                if (Items.Count != 0)
                {
                    Items = new List<T>();
                    dirty = true;
                }
            }
        }
        internal void AddItem(T item)
        {
            lock (this)
            {
                Items.Add(item);
                dirty = true;
            }
        }

        internal void AddItems(IEnumerable<T> items)
        {
            lock (this)
            {
                foreach (var item in items)
                {
                    Items.Add(item);
                }
                dirty = true;
            }

        }


        internal virtual void Refresh()
        {
            if (!dirty)
                return;

            lock (this)
            {
                Factory.SetItems(Items);
                dirty = false;
            }
        }
    }
}
