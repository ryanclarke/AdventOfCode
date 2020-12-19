using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using FluentAssertions;

namespace AoC2020
{
    [Command("day19")]
    public class Day19 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\19.txt")
                .ToArray();
            
            var (r31, r42) = CondenseRulesToRegex(input);
            
            input.Count(new Regex($"^{r42}{r42}{r31}$").IsMatch)
                .Dump().Should().Be(156);
            
            Enumerable.Range(1, 4)
                .Select(i => new Regex($"^{r42}+{r42}{{{i}}}{r31}{{{i}}}$"))
                .Select(r => input.Count(r.IsMatch))
                .Sum()
                .Dump().Should().Be(363);
            
            return default;
        }

        private static (string R31, string R42) CondenseRulesToRegex(IEnumerable<string> input)
        {
            var avoidRules = new[] {"<0>", "<8>", "<11>", "<31>", "<42>"};
            
            var rules = input
                .Where(line => Regex.IsMatch(line, @"^\d"))
                .Select(line => line
                    .Split(":")
                    .Map(a => (
                        key: $"<{a[0]}>",
                        val: a[1] switch
                        {
                            " \"a\"" => "a",
                            " \"b\"" => "b",
                            var x => Regex.Replace(x, @" (\d+)", "<$1>").Map(s => $"({s})")
                        })))
                .ToDictionary(r => r.key, r => r.val);

            var hasUnresolvedReference = new Regex(@"\d");

            var keepGoing = true;
            while (keepGoing)
            {
                var sub = rules
                    .Where(r => !(avoidRules.Contains(r.Key)))
                    .First(r => !hasUnresolvedReference.IsMatch(r.Value));
                rules = rules
                    .Where(r => r.Key != sub.Key)
                    .Select(r => (
                        r.Key, 
                        Value: r.Value.Replace(sub.Key, sub.Value).Replace(" ", "")))
                    .ToDictionary(r => r.Key, r => r.Value);
                keepGoing = !rules.All(r => avoidRules.Contains(r.Key));
            }

            return (rules["<31>"], rules["<42>"]);
        }
    }
}