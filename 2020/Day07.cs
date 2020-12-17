using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day07")]
    public class Day07 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\07.txt")
                .ToList();

            var rules = input
                .Select(ParseRule)
                .ToList();

            var edges = rules
                .SelectMany(r => r.Contents.Select(c => new Edge(r.Style, c.Style, c.Number)));

            var styles = new string[] {};
            var newStyles = new [] {"shiny gold"};
            while (newStyles.Length > 0)
            {
                styles = styles
                    .Concat(newStyles)
                    .Distinct()
                    .ToArray();
                newStyles = edges
                        .Where(e => newStyles.Contains(e.Inner))
                        .Select(r => r.Outer)
                        .Distinct()
                        .ToArray();
            }
            (styles.Length - 1).Dump();

            var found = Array.Empty<Contents>();
            var layer = new[] {new Contents(1, "shiny gold")};
            while (layer.Length > 0)
            {
                var contents = layer
                    .SelectMany(l => rules
                        .Find(r => l.Style.Contains(r.Style))
                        .Contents
                        .Select(c => c with {Number = c.Number * l.Number}))
                    .ToArray();
                found = found.Concat(contents).ToArray();
                layer = contents;
            }
            found.Sum(c => c.Number).Dump();

            return default;
        }

        private Rule ParseRule(string line)
        {
            var matches = Regex.Match(line, @"(?<Outer>.+) bags contain (?<Inner>.*)");
            var outer = matches.Groups["Outer"].Value;
            var inners =
                Regex.Matches(matches.Groups["Inner"].Value, @"(?<Number>\d) (?<Style>\w+ \w+) bags?")
                    .Select(x => new Contents(int.Parse(x.Groups["Number"].Value), x.Groups["Style"].Value))
                    .ToList();

            return new Rule(outer, inners);
        }

        public record Edge(string Outer, string Inner, int Number);
        public record Rule(string Style, List<Contents> Contents);
        public record Contents(int Number, string Style);
    }
}