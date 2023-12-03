using Draw = (int Count, string Color);
using Game = (int Id, System.Collections.Generic.List<(int Count, string Color)> Draws);

public class Day02
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/02.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var games = input
          .Select(l => l.Trim().Split(':')
            .X(a => (
              Id: int.Parse(a[0].Trim().Split(' ')[1]),
              Draws: a[1].Trim().Split(';', ',')
                  .Select(t => t.Trim().Split(' ')
                    .X(z => (Count: int.Parse(z[0]), Color: z[1])))
                .ToList())))
          .ToList();
        
        var possible = new Dictionary<string, int>{
          { "red", 12 },
          { "green", 13 },
          { "blue", 14 }
        };

        games.Where(g => g.Draws.All(t => possible[t.Color] >= t.Count))
          .Select(g => g.Id)
          .Sum()
          .Dump("02a [2006]: ");

        var t = games
          .Select(g => g.Draws
            .GroupBy(c => c.Color)
            .Select(g => g.MaxBy(c => c.Count).Count)
            .Aggregate(1, (a, i) => a * i))
          .Sum()
          .Dump("02b [84911]: ");
    }
}
