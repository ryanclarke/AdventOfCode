using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day10")]
    public class Day10 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\10.txt")
                .Select(int.Parse)
                .ToArray();

            var mine = input.Max() + 3;
            var adapters = input
                .Concat(new[] {0, mine})
                .OrderBy(i => i)
                .ToArray();

            var spreads = adapters[..^1]
                .Zip(adapters[1..])
                .Select(t => (t, (t.Second - t.First)))
                .ToArray()
                .GroupBy(i => i.Item2)
                .ToDictionary(
                    g => g.Key,
                    g => g.Count());
            (spreads[1] * spreads[3]).Dump();

            var nextAdapters = adapters
                .ToDictionary(
                    a => a,
                    a => adapters
                        .Where(a2 => new[] {1, 2, 3}
                            .Contains(a2 - a))
                        .ToArray());

            var possibilities = new Dictionary<int, long>();
            foreach (var adapter in adapters.Reverse())
            {
                possibilities[adapter] = adapter switch
                {
                    _ when adapter == mine => 1L,
                    _ => nextAdapters[adapter].Sum(a => possibilities[a])
                };
            }
            possibilities[0].Dump();

            return default;
        }
    }
}