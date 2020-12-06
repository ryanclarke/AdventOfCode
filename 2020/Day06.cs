using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CliFx.Attributes;
using AoC2020.Utils;

namespace AoC2020
{
    [Command("day06")]
    public class Day06 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllText("input/06.txt")
                .Split("\n\n")
                .ToList();

            input.Sum(UnionOfGroupAnswers).Dump();
            input.Sum(IntersectionOfGroupAnswers).Dump();

            return default;
        }

        private static int IntersectionOfGroupAnswers(string groupAnswers) => 
            groupAnswers
                .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
                .Select(x => x.OrderBy(c => c).ToArray())
                .Aggregate(null as char[],
                    (acc, x) =>
                        acc is null
                            ? x
                            : acc.Intersect(x).ToArray())
                .Count();
        
        private static int UnionOfGroupAnswers(string groupAnswers) =>
            groupAnswers
                .Replace("\n", "")
                .Trim()
                .Distinct()
                .Count();
    }
}