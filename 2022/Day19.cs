using System.Text.RegularExpressions;

namespace AoC2022;

public partial class Day19
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/19i.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var blueprints = input
            .Select(line => IntRegex().Matches(line).Select(m => m.Value).Select(int.Parse).ToArray())
            .Select(m => new Blueprint(m[0], new List<Cost>
            {
                new Cost(Robot.Ore, new Inventory(m[1], 0, 0, 0)),
                new Cost(Robot.Clay, new Inventory(m[2], 0, 0, 0)),
                new Cost(Robot.Obsidian, new Inventory(m[3], m[4], 0, 0)),
                new Cost(Robot.Geode, new Inventory(m[5], 0, m[6], 0)),
            }))
            .ToList();


        var z = 12;
    }

    private record Blueprint(int Id, List<Cost> Costs);
    private record Cost(Robot Robot, Inventory Inventory);
    private record Inventory(int Ore, int Clay, int Obsidian, int Geode);
    private enum Robot
    {
        Ore,
        Clay,
        Obsidian,
        Geode
    }

    [GeneratedRegex("\\d+")]
    private static partial Regex IntRegex();
}
