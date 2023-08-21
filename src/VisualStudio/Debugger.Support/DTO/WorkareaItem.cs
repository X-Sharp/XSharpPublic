
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Debugger.Support
{
    [DebuggerDisplay("{Area} {Alias,nq} {RDD,nq}")]
    public sealed class WorkareaItem
    {
        public int Area { get; set; }
        public string Alias { get; set; }
        public string RDD { get; set; }
        public bool Selected { get; set; }
    }

    public sealed class WorkareaItems
    {
        private List<WorkareaItem> items;
        public WorkareaItems()
        {
            items = new List<WorkareaItem>(); 
        }
        public WorkareaItem[] Items
        {
            get
            {
                return items.ToArray();
            }
            set
            {
                items = new List<WorkareaItem>();
                items.AddRange(value);
            }
        }
        public void Add(WorkareaItem item)
        {
            items.Add(item);
        }
        public string Serialize()
        {
            var sb = new StringBuilder();
            bool first = true;
            foreach (var item in this.Items)
            {
                if (first)
                    first = false;
                else
                    sb.Append("|");
                sb.Append(item.Area);
                sb.Append(",");
                sb.Append(item.Alias);
                sb.Append(",");
                sb.Append(item.RDD);
                sb.Append(",");
                sb.Append(item.Selected);
            }
            return sb.ToString();
        }
        public static WorkareaItems Deserialize(string str)
        {
            var result = new WorkareaItems();
            var items = str.Split('|');
            foreach (var item in items)
            {
                var fields = item.Split(',');
                if (fields.Length >= 4)
                {
                    var witem = new WorkareaItem();
                    Int32.TryParse(fields[0], out var iArea);
                    witem.Area = iArea;
                    witem.Alias = fields[1];
                    witem.RDD = fields[2];
                    witem.Selected = fields[3] == "True";
                    result.Add(witem);
                }
            }
            return result;
        }
    }

}
