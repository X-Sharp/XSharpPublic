
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Debugger.Support
{
    [DebuggerDisplay("{Name,nq} {Value,nq}")]
    public sealed class GlobalItem
    {
        public string Name { get; set; }
        public string Assembly { get; set; }
        public string Value { get; set; }
    }

    public sealed class GlobalItems
    {
        private List<GlobalItem> items;
        public GlobalItems()
        {
            items = new List<GlobalItem>(); 
        }
        public GlobalItem[] Items
        {
            get
            {
                return items.ToArray();
            }
            set
            {
                items = new List<GlobalItem>();
                items.AddRange(value);
            }
        }
        public void Add(GlobalItem item)
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
                sb.Append(item.Name);
                sb.Append(",");
                sb.Append(item.Assembly);
                sb.Append(",");
                sb.Append(item.Value);
            }
            return sb.ToString();
        }
        public static GlobalItems Deserialize(string str)
        {
            var result = new GlobalItems();
            var items = str.Split('|');
            foreach (var item in items)
            {
                var fields = item.Split(',');
                if (fields.Length >= 3)
                {
                    var sitem = new GlobalItem();
                    sitem.Name = fields[0];
                    sitem.Assembly = fields[1];
                    sitem.Value = fields[2];
                    result.Add(sitem);
                }
            }
            return result;
        }
    }

}
