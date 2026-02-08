using Debugger.Support;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Debugger.Support
{
    public class NameValueItem
    {
        public string Name { get; set; }
        public string Value { get; set; }
        public string Length { get; set; }
        public string Decimals { get; set; }
    }
    public sealed class NameValueItems
    {
        public bool Sorted = false;
        private List<NameValueItem> items;
        public NameValueItems()
        {
            items = new List<NameValueItem>();
        }
        public NameValueItem[] Items
        {
            get
            {
                var result = items.ToArray();
                if (Sorted)
				{
                    Array.Sort(result, (x, y) => x.Name.CompareTo(y.Name));
				}	
                return result;
            }
            set
            {
                items = new List<NameValueItem>();
                items.AddRange(value);
            }
        }
        public void Add(NameValueItem item)
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
                sb.Append(item.Value);
                sb.Append(",");
                sb.Append(item.Length);
                sb.Append(",");
                sb.Append(item.Decimals);
            }
            return sb.ToString();
        }
        public static NameValueItems Deserialize(string str)
        {
            var result = new NameValueItems();
            var items = str.Split('|');
            foreach (var item in items)
            {
                var fields = item.Split(',');
                if (fields.Length >= 4)
                {
                    var nvitem = new NameValueItem();
                    nvitem.Name = fields[0];
                    nvitem.Value = fields[1];
                    nvitem.Length = fields[2];
                    nvitem.Decimals = fields[3];
                    result.Add(nvitem);
                }
            }
            return result;
        }
    }
}
