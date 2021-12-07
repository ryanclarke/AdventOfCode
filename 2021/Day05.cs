namespace AoC2021;

public class Day05
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/05.txt")
                .Select(line => line.Split(" -> ").λ(x => (
                    x[0].Split(',').λ(a => (int.Parse(a[0]), int.Parse(a[1]))), 
                    x[1].Split(',').λ(a => (int.Parse(a[0]), int.Parse(a[1]))))))
                .ToList();

        input.SelectMany(p => p switch
            {
                ((int x1, int y1), (int x2, int y2)) when x1 == x2 => Enumerable.Range(y1 < y2 ? y1 : y2, Math.Abs(y1 - y2) + 1).Select(y => new Point(x1, y)).ToArray(),
                ((int x1, int y1), (int x2, int y2)) when y1 == y2 => Enumerable.Range(x1 < x2 ? x1 : x2, Math.Abs(x1 - x2) + 1).Select(x => new Point(x, y1)).ToArray(),
                ((int x1, int y1), (int x2, int y2)) => Array.Empty<Point>()
            })
            .GroupBy(a => a)
            .Where(g => g.Count() > 1)
            .Count()
            .Dump("5a (6007): ");



        input.SelectMany(p =>
            {
                var ((x1, y1), (x2, y2)) = p;
                var xs = (x1, x2) switch
                {
                    (int a, int b) when a > b => Enumerable.Range(b, a - b + 1).Reverse(),
                    (int a, int b) when b > a => Enumerable.Range(a, b - a + 1),
                    _ => Enumerable.Repeat(x1, Math.Abs(y2 - y1) + 1)
                };
                var ys = (y1, y2) switch
                {
                    (int a, int b) when a > b => Enumerable.Range(b, a - b + 1).Reverse(),
                    (int a, int b) when b > a => Enumerable.Range(a, b - a + 1),
                    _ => Enumerable.Repeat(y1, Math.Abs(x2 - x1) + 1)
                };
                return Enumerable.Zip(xs, ys, (x, y) => new Point(x, y));
            })
            .GroupBy(a => a)
            .Where(g => g.Count() > 1)
            .Count()
            .Dump("5b (19349): ");
    }

    public record Point(int X, int Y);
}
