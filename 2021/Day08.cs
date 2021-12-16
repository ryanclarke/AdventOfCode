namespace AoC2021;

public class Day08
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/08.txt")
                .Select(l => l.Split(" | ", StringSplitOptions.RemoveEmptyEntries & StringSplitOptions.TrimEntries)
                    .Select(h => h.Split(' ', StringSplitOptions.RemoveEmptyEntries & StringSplitOptions.TrimEntries))
                    .ToArray()
                    .X(p => (Patterns: p[0], Output: p[1])))
                .ToList();

        input.SelectMany(t => t.Output)
            .Count(o => o.Length is < 5 or 7)
            .Dump("8a (278): ");

        input.Sum(t =>
            {
                var p = t.Patterns
                    .Select(p => new string(p.OrderBy(x => x).ToArray()))
                    .OrderBy(p => p.Length)
                    .ToArray();

                var _1 = p[0];
                var _7 = p[1];
                var _4 = p[2];
                var _8 = p[9];

                var fives = p.Where(x => x.Length == 5).ToList();
                var _3 = fives.Single(x => _1.Intersect(x).Count() == 2);
                fives.Remove(_3);
                var _2 = fives.Single(x => _4.Intersect(x).Count() == 2);
                fives.Remove(_2);
                var _5 = fives.Single();

                var sixes = p.Where(x => x.Length == 6).ToList();
                var _6 = sixes.Single(x => _1.Intersect(x).Count() == 1);
                sixes.Remove(_6);
                var _9 = sixes.Single(x => _4.Intersect(x).Count() == 4);
                sixes.Remove(_9);
                var _0 = sixes.Single();

                var d = new Dictionary<string, char>
                {
                    { _0, '0' },
                    { _1, '1' },
                    { _2, '2' },
                    { _3, '3' },
                    { _4, '4' },
                    { _5, '5' },
                    { _6, '6' },
                    { _7, '7' },
                    { _8, '8' },
                    { _9, '9' }
                };

                return new string(t.Output.Select(o => new string(o.OrderBy(x => x).ToArray())).Select(x => d[x]).ToArray()).X(int.Parse);
            })
            .Dump("8b (986179): ");
    }
}
