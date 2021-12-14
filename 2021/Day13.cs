namespace AoC2021;

public class Day13
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/13.txt")
                .ToList();

        var points = input
            .Where(line => line.Contains(','))
            .Select(line => line.Split(',').λ(a => new Point(X: int.Parse(a[0]), Y: int.Parse(a[1]))))
            .ToHashSet();
        var folds = input
            .Where(line => line.StartsWith("fold along"))
            .Select(line => line.Split('=').λ(a => (
                Orientation: a[0].Last().ToString(), 
                Line: int.Parse(a[1]))))
            .ToList();

        Fold(points, folds.First())
            .Count()
            .Dump("13a (621): ");

        var final = folds.Aggregate(points, (ps, f) => Fold(ps, f));
        Console.WriteLine("13b (HKUJGAJZ): ");
        for (int y = 0; y <= final.MaxBy(p => p.Y)!.Y; y++)
        {
            for (int x = 0; x <= final.MaxBy(p => p.X)!.X; x++)
            {
                Console.Write(final.Contains(new Point(x, y)) ? "█" : " ");
            }
            Console.WriteLine();
        }

        HashSet<Point> Fold(HashSet<Point> points, (string Orientation, int Line) fold) => 
            points.Select(p => fold.Orientation switch
                {
                    "y" => p with { Y = p.Y > fold.Line ? fold.Line * 2 - p.Y : p.Y },
                    "x" => p with { X = p.X > fold.Line ? fold.Line * 2 - p.X : p.X },
                    _ => throw new NotImplementedException()
                })
                .ToHashSet();
    }

    public record Point(int X, int Y);
}
