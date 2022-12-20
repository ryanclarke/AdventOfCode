using System.Numerics;

namespace AoC2022;

public class Day18
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/18.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var points = input
            .Select(line => line
                .Split(',')
                .Select(int.Parse)
                .ToArray()
                .X(a => new Vector3(a[0], a[1], a[2])))
            .ToList();

        var neighbors = new []
        {
            Vector3.UnitX * 1,
            Vector3.UnitX * -1,
            Vector3.UnitY * 1,
            Vector3.UnitY * -1,
            Vector3.UnitZ * 1,
            Vector3.UnitZ * -1,
        };

        points
            .Sum(point => neighbors
                .Count(neighbor => !points.Contains(point + neighbor)))
            .Dump("18a (3326): ");

    }
}
