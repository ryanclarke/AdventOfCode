using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using CliFx;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day04")]
    public class Day04 : ICommand
    {
        public ValueTask ExecuteAsync(IConsole console)
        {
            var input = File
                .ReadAllText(@"C:\dev\AdventOfCode\2020\input\04.txt")
                .Split("\n\n")
                .Select(a => a.Replace("\n", " ").Trim())
                .ToList();

            var requiredFields = new List<string>
            {
                "byr",
                "iyr",
                "eyr",
                "hgt",
                "hcl",
                "ecl",
                "pid"
            };

            var validA = input
                .Select(p => p
                    .Split(" ")
                    .Select(x => x.Split(':'))
                    .ToDictionary(x => x[0], x => x[1]))
                .Where(p => requiredFields.All(p.ContainsKey))
                .ToList();
            
            console.Output.WriteLine(validA.Count);

            var validB = validA
                .Where(IsValid)
                .ToList();
            
            console.Output.WriteLine(validB.Count);

            return default;
        }

        private static bool IsValid(Dictionary<string, string> fields)
        {
            return fields.All(IsValid);
        }

        private static bool IsValid(KeyValuePair<string, string> field)
        {
            return field.Key switch
            {
                "byr" => IsIntBetween(field.Value, 1920, 2002),
                "iyr" => IsIntBetween(field.Value, 2010, 2020),
                "eyr" => IsIntBetween(field.Value, 2020, 2030),
                "hgt" => IsValidHeight(field.Value),
                "hcl" => Regex.IsMatch(field.Value, "^#[0-9a-f]{6}$"),
                "ecl" => new List<string> {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}.Contains(field.Value),
                "pid" => Regex.IsMatch(field.Value, @"^\d{9}$"),
                "cid" => true,
                { } x => throw new Exception(x)
            };
        }

        private static bool IsValidHeight(string value)
        {
            var match = Regex.Match(value, @"^(?<size>\d{2,3})(?<unit>cm|in)$");
            return match.Success && match.Groups["unit"].Value switch
            {
                "cm" => IsIntBetween(match.Groups["size"].Value, 150, 193),
                "in" => IsIntBetween(match.Groups["size"].Value, 59, 76),
                { } x => throw new Exception(x)
            };
        }

        private static bool IsIntBetween(string value, int begin, int end) =>
            int.TryParse(value, out int i) && i >= begin && i <= end;
    }
}