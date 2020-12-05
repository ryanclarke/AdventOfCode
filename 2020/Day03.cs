using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CliFx;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day03")]
    public class Day03 : ICommand
    {
        private static readonly List<string> Input = File
            .ReadAllLines("input/03.txt")
            .ToList();

        public ValueTask ExecuteAsync(IConsole console)
        {
            var trees = new[]
            {
                (1, 1),
                (1, 3),
                (1, 5),
                (1, 7),
                (2, 1)
            }
                .Select(TreesForSlope)
                .ToList();
            
            console.Output.WriteLine(trees[1]);
            console.Output.WriteLine(trees.Aggregate((a, t) => a * t));

            return default;
        }

        private static long TreesForSlope((int down, int right) slope) =>
            Input
                .Where((_, i) => i % slope.down == 0)
                .Select(x => x.ToCharArray())
                .Select((x, i) => x[(i * slope.right) % x.Length])
                .Count(x => x == '#');
    }
}