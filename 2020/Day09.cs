using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using MoreLinq;

namespace AoC2020
{
    [Command("day09")]
    public class Day09 : AocCommand
    {
        private const int PreambleLength = 25;
        private long[] _input;

        protected override ValueTask Run()
        {
            _input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\09.txt")
                .Select(long.Parse)
                .ToArray();

            var weakness = _input
                .Skip(PreambleLength)
                .Where(DoesntHaveXmasEncryptionProperty)
                .First();

            weakness.Dump();

            _input
                .Select((l, i) =>
                    {
                        var j = 0;
                        var nums = new long[]{};
                        while (nums.Sum() < weakness)
                        {
                            j++;
                            nums = _input[i..(i + j)];
                        }

                        return nums.Sum() == weakness
                            ? nums.Min() + nums.Max()
                            : 0L;
                    }
                )
                .First(x => x > 0)
                .Dump();
            
            return default;
        }

        private bool DoesntHaveXmasEncryptionProperty(long number, int idx)
        {
            var preamble = _input[idx..(idx+PreambleLength)];
            var xmasPossibilities = preamble[..^2]
                .SelectMany((a, aIdx) =>
                    preamble[(aIdx + 1)..]
                        .Select(b => a + b))
                .ToArray();
            return !xmasPossibilities.Contains(number);
        }
    }
}