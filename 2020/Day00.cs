using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CliFx;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day00")]
    public class Day00 : ICommand
    {
        public ValueTask ExecuteAsync(IConsole console)
        {
            var input = File
                .ReadAllLines("input/00.txt")
                .ToList();

            return default;
        }
    }
}