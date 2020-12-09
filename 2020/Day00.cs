using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day00")]
    public class Day00 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines("input/00.txt")
                .ToList();
            
            

            return default;
        }
    }
}