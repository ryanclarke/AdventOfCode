namespace AoC2021;

public static class Day23
{
    public static List<string> Input = new List<string>();
    public static HashSet<Point> Graph = new HashSet<Point>();
    public static HashSet<Point> InvalidHallwaySpaces = new() { new(3, 1), new(5, 1), new(7, 1), new(9, 1) };
    public static int LowestCost = int.MaxValue;


    // b <47609
    // 47905, 47533
    public static void Run()
    {
        //SolveFile("23").Dump("23a (19019): ");
        SolveFile("23b").Dump("23b (47533): ");
    }

    public static int SolveFile(string fileName) {
        Input = File
                .ReadAllLines($"../../../input/{fileName}.txt")
                .ToList();

        Graph = Input
            .SelectMany((line, y) => line
                .Select((c, x) => new[] { '.', 'A', 'B', 'C', 'D' }.Contains(c) ? new Point(x, y) : new Point(0,0))
                .Where(p => p.Y != 0))
            .ToHashSet();

        var amphipods = Input
            .SelectMany((line, y) => line
                .Select((c, x) => c switch
                {
                    'A' => new Amphipod(c, new(x, y), new(x, y), new(3, Input.Count - 2), 1),
                    'B' => new Amphipod(c, new(x, y), new(x, y), new(5, Input.Count - 2), 10),
                    'C' => new Amphipod(c, new(x, y), new(x, y), new(7, Input.Count - 2), 100),
                    'D' => new Amphipod(c, new(x, y), new(x, y), new(9, Input.Count - 2), 1000),
                    _ => new Amphipod(' ', new(0, 0), new(0, 0), new(0, 0), 0)
                })
                .Where(p => p.Cost != 0))
            .ToList();

        return Solve(amphipods);
    }

    public static void Print(List<Amphipod> amphipods, int steps, int cost)
    {
        Enumerable.Range(0, Input.Count)
            .Select(y => Enumerable.Range(0, Input[0].Length)
                .Select(x => Graph.Contains(new(x, y))
                    ? amphipods.FirstOrDefault(a => a.Point == new Point(x, y))?.Char ?? '.'
                    : ' ')
                .Stringify())
            .JoinLines()
            .Dump($"\nStep: {steps:00}, Cost: {cost:00000}, LowestCost: {LowestCost}\n");
    }

    public static int Solve(List<Amphipod> amphipods, int steps = 0, int cost = 0)
    {
        if (amphipods.All(a => a.IsFinished()))
        {
            if (cost < LowestCost)
            {
                LowestCost = cost;
                LowestCost.Dump();
            }
            return cost;
        }

        var costs = new List<int>();

        // Go to finishing spot
        var potentialFinishers = amphipods
                    .Where(a => !a.IsFinished())
                    .Select(a => (a, a.Point.Search(a.Target, amphipods)))
                    .Where(t => t.Item2 < int.MaxValue)
                    .ToList();
        foreach (var (amphipod, dist) in potentialFinishers)
        {
            var newCost = cost + dist * amphipod.Cost;
            if (newCost < LowestCost)
            {
                var newAmphipods = amphipods.Select(a =>
                    a.StartingPoint == amphipod.StartingPoint
                        ? a with { Point = a.Target }
                        : a.Char == amphipod.Char && !a.IsFinished()
                            ? a with { Target = a.Target with { Y = a.Target.Y - 1 } }
                            : a)
                    .ToList();
                costs.Add(Solve(newAmphipods, steps + 1, newCost));
            }
        }
        if (costs.Any())
        {
            return costs.Min();
        }

        // Go to hallway
        var potentialMovers = amphipods
                    .Where(a => !a.IsFinished() && !a.HasMoved())
                    .ToList();
        foreach (var amphipod in potentialMovers)
        {
            var potentialTargets = Graph
                            .Where(p => p.Y == 1 && !InvalidHallwaySpaces.Contains(p) && !p.Full(amphipods))
                            .Select(p => (p, amphipod.Point.Search(p, amphipods)))
                            .Where(t => t.Item2 < int.MaxValue)
                            .ToList();
            foreach (var (potentialTarget, dist) in potentialTargets)
            {
                var newCost = cost + dist * amphipod.Cost;
                if (newCost < LowestCost)
                {
                    var newAmphipods = amphipods.Select(a =>
                        a.StartingPoint == amphipod.StartingPoint
                            ? a with { Point = potentialTarget }
                            : a)
                        .ToList();
                    costs.Add(Solve(newAmphipods, steps + 1, newCost));
                }
            }
        }
        return costs.Any() ? costs.Min() : int.MaxValue;
    }

    public record Amphipod(Char Char, Point StartingPoint, Point Point, Point Target, int Cost)
    {
        public bool HasMoved() => Point != StartingPoint;
        public bool IsFinished() => Point == Target;
    };

    public record Point(int X, int Y);
    public static HashSet<Point> Neighbors(this Point point)
    {
        var (x, y) = (point.X, point.Y);
        return new Point[]
        {
                new Point(x,     y - 1),
                new Point(x - 1, y),
                new Point(x + 1, y),
                new Point(x,     y + 1)
        }
        .Where(Graph.Contains)
        .ToHashSet();
    }

    public static bool Full(this Point point, List<Amphipod> amphipods) => amphipods.Any(a => a.Point == point);

    public static int Search(this Point source, Point target, List<Amphipod> amphipods)
    {
        var distance = Graph.ToDictionary(p => p, _ => int.MaxValue);
        var Q = new Queue<Point>();
        Q.Enqueue(source);
        distance[source] = 0;
        while (Q.Count > 0)
        {
            var current = Q.Dequeue();
            foreach (var neighbor in current.Neighbors().Where(p => !p.Full(amphipods)))
            {
                var dist = distance[current] + 1;
                if (distance[neighbor] > dist)
                {
                    distance[neighbor] = dist;
                    Q.Enqueue(neighbor);
                }
            }
        }

        return distance[target];
    }
}
