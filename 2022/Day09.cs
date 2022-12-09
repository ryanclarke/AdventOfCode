using static System.Math;

namespace AoC2022;

public class Day09
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/09.txt";
    public static void Run()
    {
        const int knots = 10;
        var rope = Enumerable.Repeat(YX.Zero, knots).ToArray();
        var locations = rope.Select(knot => new HashSet<YX>{knot}).ToArray();
        foreach (var (dir, dist) in File
                     .ReadAllLines(InputFilePath)
                     .Select(line => line
                        .Split()
                        .X(a => (YX.Parse(a[0]), int.Parse(a[1])))))
        {
            for (var i = 0; i < dist; i++)
            {
                rope[0] += dir;
                locations[0].Add(rope[0]);
                for (var knot = 1; knot < knots; knot++)
                {
                    var diff = rope[knot - 1] - rope[knot];
                    if (diff.NotTouching())
                    {
                        rope[knot] += diff.Sign();
                    }
                    locations[knot].Add(rope[knot]);
                }
            } 
        }
        
        locations[1].Count().Dump("09a (5779): ");
        locations[9].Count().Dump("09b (2331): ");
    }

    private record YX(int Y, int X)
    {
        public static readonly YX Zero = new(0, 0);

        public static YX Parse(string direction) => direction switch
        {
            "D" => new YX(-1, 0),
            "L" => new YX(0, -1),
            "R" => new YX(0, 1),
            "U" => new YX(1, 0),
            _ => YX.Zero
        };
        
        public static YX operator +(YX a, YX b) => new(a.Y + b.Y, a.X + b.X);
        public static YX operator -(YX a, YX b) => new(a.Y - b.Y, a.X - b.X);

        public YX Sign() => new(Math.Sign(Y), Math.Sign(X));
        public bool NotTouching() => Abs(Y) == 2 || Abs(X) == 2;
    };
}
