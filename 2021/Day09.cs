namespace AoC2021;

public class Day09
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/09.txt")
                .SelectMany((l, y) => l.Select((c, x) => (new Point(x, y), int.Parse(c.ToString()))))
                .ToDictionary(l => l.Item1, l => l.Item2);

        var neighbors = new List<Point>()
                {
                    new Point(-1, 0),
                    new Point(1, 0),
                    new Point(0, -1),
                    new Point(0, 1)
                };
        var lowPoints = input
            .Where(l => neighbors
                .Select(p => p + l.Key)
                .Where(input.ContainsKey)
                .All(p => input[p] > l.Value))
            .ToList(); ;
        lowPoints
            .Sum(l => l.Value + 1)
            .Dump("09a (594): ");

        lowPoints.Select(l => Area(l.Key))
            .OrderByDescending(x => x)
            .Take(3)
            .Aggregate(1, (a, x) => a * x)
            .Dump("09b (858494): ");


        int Area(Point p)
        {
            var ps = new HashSet<Point> { p };
            var ns = new HashSet<Point>();

            do
            {
                ps = ps.Union(ns).ToHashSet();
                ns = ps.SelectMany(x => neighbors
                        .Select(n => n + x)
                        .Where(input.ContainsKey)
                        .Where(x => input[x] != 9))
                    .ToHashSet();
            } while (ps.Union(ns).Count() > ps.Count);

            return ps.Count;
        };
    }


    public record Point(int X, int Y)
    {
        public static Point operator +(Point a, Point b) => new(a.X + b.X, a.Y + b.Y);
    }
}
