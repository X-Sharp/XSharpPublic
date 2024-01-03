
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
        public bool EoF { get; set; }
        public bool Found { get; set; }
        public bool BoF { get; set; }
        public int RecNo { get; set; }
        public int RecCount { get; set; }
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
                sb.Append(item.EoF);
                sb.Append(",");
                sb.Append(item.BoF);
                sb.Append(",");
                sb.Append(item.Found);
                sb.Append(",");
                sb.Append(item.RecNo);
                sb.Append(",");
                sb.Append(item.RecCount);
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
                if (fields.Length >= 8)
                {
                    var witem = new WorkareaItem();
                    Int32.TryParse(fields[0], out var iArea);
                    witem.Area = iArea;
                    witem.Alias = fields[1];
                    witem.RDD = fields[2];
                    witem.EoF = fields[3] == "True";
                    witem.BoF = fields[4] == "True";
                    witem.Found  = fields[5] == "True";
                    Int32.TryParse(fields[6], out var iRecno);
                    witem.RecNo = iRecno;
                    Int32.TryParse(fields[7], out var iCount);
                    witem.RecCount = iCount;
                    witem.Selected = fields[8] == "True";
                    result.Add(witem);
                }
            }
            return result;
        }
    }

}
