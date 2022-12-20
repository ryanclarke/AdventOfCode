using System.Linq.Expressions;
using System.Text.RegularExpressions;
using Microsoft.VisualBasic;

namespace AoC2022;

public class Day16
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/16.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var valves = input
            .Select(line =>
            {
                var (name, connects) = Regex
                        .Matches(line, @"\b[A-Z]{2}\b")
                        .Select(m => m.Value)
                        .ToArray() switch
                        {
                            [var first, .. var rest] => (first, rest),
                            _ => throw new Exception()
                        };
                var flowRate = int.Parse(Regex.Match(line, @"[\d]+").Value);
                return new Valve(name, flowRate, connects);
            })
            .ToDictionary(v => v.Name, ID);

        var minute = 0;
        var currentName = "AA";

        Visit(currentName).Dump();
    }

    private static int Visit(string Name)
    {
        return 0;
    }

    private record Valve(string Name, int FlowRate, string[] Connects);
}
