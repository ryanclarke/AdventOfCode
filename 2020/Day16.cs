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
    [Command("day16")]
    public class Day16 : AocCommand
    {
        private static readonly Regex RuleRegex =
            new Regex(@"^(?<Name>[\w ]+): (?<Min1>\d+)-(?<Max1>\d+) or (?<Min2>\d+)-(?<Max2>\d+)$");
        private static readonly Regex YourTicketRegex =
            new Regex(@"^your ticket:");
        private static readonly Regex TicketRegex =
            new Regex(@"^(\d+,)+\d+$");

        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\16.txt")
                .ToArray();


            var rules = new List<Rule>();
            var yourTicket = new List<int>();
            var nearbyTickets = new List<List<int>>();
            foreach (var line in input)
            {
                switch (line)
                {
                    case var _ when RuleRegex.IsMatch(line):
                        var r = RuleRegex.Match(line);
                        rules.Add(new Rule(
                            r.Groups["Name"].Value,
                            new []
                            {
                                new Range(
                                    r.Groups["Min1"].Value.Map(int.Parse),
                                    r.Groups["Max1"].Value.Map(int.Parse)),
                                new Range(
                                    r.Groups["Min2"].Value.Map(int.Parse),
                                    r.Groups["Max2"].Value.Map(int.Parse))
                            }));
                        break;
                    case var _ when TicketRegex.IsMatch(line):
                        var ticket = line
                            .Split(',')
                            .Select(int.Parse)
                            .ToList();
                        if (yourTicket.Any())
                        {
                            nearbyTickets.Add(ticket);
                        }
                        else
                        {
                            yourTicket = ticket;
                        }
                        break;
                }
            }

            var invalidValues = Enumerable
                .Range(0, 1000)
                .Where(val =>
                    !rules.Any(rule => rule.IsValid(val)))
                .ToList();

            nearbyTickets
                .SelectMany(t =>
                    t.Where(v => invalidValues.Contains(v)))
                .Sum()
                .Dump()
                .Should()
                .Be(22977);

            Enumerable
                .Range(0, yourTicket.Count)
                .Select(idx => rules
                    .Where(rule => nearbyTickets
                        .Where(t =>
                            t.All(v => !invalidValues.Contains(v)))
                        .ToList()
                        .Select(t => t[idx])
                        .All(rule.IsValid)))
                .Select((r, idx) => (idx, rules: r))
                .OrderBy(t => t.rules.Count())
                .Aggregate(
                    new List<(int idx, string rule)>(),
                    (state, t) =>
                        state
                            .Append((t.idx,
                                t.rules
                                    .First(r => state.All(s => s.rule != r.Name))
                                    .Name))
                            .ToList())
                .Where(t => t.rule.StartsWith("departure"))
                .Select(t => yourTicket[t.idx])
                .Aggregate(1L, (prod, i) => prod * (long)i)
                .Dump()
                .Should()
                .Be(998358379943L);

            return default;
        }

        public record Rule(string Name, IEnumerable<Range> Ranges);

        public record Range(int Min, int Max);
    }

    public static class Day16Utils
    {
        public static bool IsValid(this Day16.Rule rule, int val) =>
            rule.Ranges.Any(range =>
                val >= range.Min && val <= range.Max
            );
    }
}