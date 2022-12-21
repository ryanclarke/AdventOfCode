namespace AoC2021;

public class Day25
{
    public static int Height = 0;
    public static int Width = 0;
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/25.txt")
                .ToList();

        Height = input.Count;
        Width = input[0].Length;
        var cukes = input
            .SelectMany((line, y) => line
                .SelectMany((c, x) => c switch
                {
                    '>' => new[] { new SeaCucumber(true, x, y) },
                    'v' => new[] { new SeaCucumber(false, x, y) },
                    _ => Array.Empty<SeaCucumber>(),
                }))
            .ToList();
        var dCukes = cukes.ToDictionary(c => new Point(c.X, c.Y), c => c.East);

        var moves = int.MaxValue;
        var steps = 0;
        while (moves > 0)
        {
            moves = 0;

            var newDCukes = new Dictionary<Point, bool>();
            foreach (var c in dCukes)
            {
                var nextPoint = c.Key.Next(c.Value);
                if (!c.Value || dCukes.ContainsKey(nextPoint))
                {
                    newDCukes.Add(c.Key, c.Value);
                }
                else
                {
                    moves++;
                    newDCukes.Add(nextPoint, c.Value);
                }
            }
            dCukes = newDCukes;
            newDCukes = new Dictionary<Point, bool>();
            foreach (var c in dCukes)
            {
                var nextPoint = c.Key.Next(c.Value);
                if (c.Value || dCukes.ContainsKey(nextPoint))
                {
                    newDCukes.Add(c.Key, c.Value);
                }
                else
                {
                    moves++;
                    newDCukes.Add(nextPoint, c.Value);
                }
            }
            dCukes = newDCukes;

            steps++;

            //Console.WriteLine($"\n{steps}");
            //Enumerable.Range(0, Height)
            //    .Select(y =>
            //        Enumerable.Range(0, Width)
            //            .Select(x => dCukes.ContainsKey(new Point(x, y)) ? dCukes[new Point(x, y)] ? '>' : 'v' : '.')
            //            .Stringify())
            //    .JoinLines()
            //    .Dump();
        }

        steps.Dump();

        //SeaCucumber TryMove(SeaCucumber cuke)
        //{
        //    var next = cuke.East switch
        //    {
        //        true => cuke with { X = (cuke.X + 1) % width },
        //        false => cuke with { Y = (cuke.Y + 1) % height }
        //    };
        //    if (cukes.Any(c => c.X == next.X && c.Y == next.Y))
        //    {
        //        return cuke;
        //    }
        //    moves++;
        //    return next;
        //}
    }

    public record SeaCucumber(bool East, int X, int Y);
    public record Point(int X, int Y)
    {
        public Point Next(bool isEast) => isEast ? new Point((X + 1) % Width, Y) : new Point(X, (Y + 1) % Height);
    }
}
