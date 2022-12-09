using static System.Linq.Enumerable;

namespace AoC2022;

public class Day08
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/08.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var ys = input.Select(line => line.ToCharArray().Select(x => x - 48).ToArray()).ToArray();
        var xs = ys.Select((_, i) => ys.Select(y => y[i]).ToArray()).ToArray();

        Range(0, ys.Length)
            .Sum(y => Range(0, xs.Length)
                .Count(x => new List<List<int>>
                {
                    ys[y].Take(x).ToList(),
                    ys[y].Skip(x+1).ToList(),
                    xs[x].Take(y).ToList(),
                    xs[x].Skip(y+1).ToList()
                }.Any(l => l.All(t => t < ys[y][x]))))
            .Dump("08a (1546): ");
        
        Range(1, ys.Length-2)
            .Max(y => Range(1, xs.Length-2)
                .Max(x => new List<List<int>>
                {
                    ys[y].Take(x).Reverse().ToList(),
                    ys[y].Skip(x+1).ToList(),
                    xs[x].Take(y).Reverse().ToList(),
                    xs[x].Skip(y+1).ToList()
                }
                .Select(l =>
                {
                    var length = l.Count;
                    var idx = l.FindIndex(t => t >= ys[y][x]);
                    return length == 0 ? 0 : idx == -1 ? length : 1 + idx;
                })
                .Aggregate(1, (acc, i) => acc * i)))
            .Dump("08b (519064): ");
    }
}
