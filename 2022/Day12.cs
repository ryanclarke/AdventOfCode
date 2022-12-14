namespace AoC2022;

public static class Day12
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/12.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var data = input
            .SelectMany((line, y) => line.Select((c, x) => (Coord: new Point(x, y), Alt: (int)c)))
            .ToList();

        var width = input.First().Length;
        var height = input.Count;
        MaxPoint = new Point(width - 1, height - 1);

        var start = data.Single(t => t.Alt == 'S').Coord;
        var end = data.Single(t => t.Alt == 'E').Coord;
        Graph = data.ToDictionary(t => t.Coord, t => t.Alt);
        Graph[start] = 'a';
        Graph[end] = 'z';
        Search(start, end)
            .Dump("12a (391): ");

        Graph
            .Where(p => p.Value == 'a')
            .Select(p => Search(p.Key, end))
            .Min()
            .Dump("12b (386): ");
    }

    public static Dictionary<Point,int> Graph = new();

    public record Point(int X, int Y);

    public static Point MaxPoint = new Point(10, 10);
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
            .Where(n => n.X >= 0 && n.X <= MaxPoint.X && n.Y >= 0 && n.Y <= MaxPoint.Y)
            .Where(n => Graph[n] <= Graph[point] + 1)
            .ToHashSet();
    }

    public static int Search(Point source, Point target)
    {
        var costs = Graph.ToDictionary(p => p.Key, _ => int.MaxValue);
        var Q = new Queue<Point>();
        Q.Enqueue(source);
        costs[source] = 0;
        while (Q.Count > 0)
        {
            var current = Q.Dequeue();
            foreach (var neighbor in current.Neighbors())
            {
                var cost = costs[current] + 1;
                if (costs[neighbor] > cost)
                {
                    costs[neighbor] = cost;
                    Q.Enqueue(neighbor);
                }
            }
        }

        return costs[target];
    }

    public static int Dijkstra(Point source, Point target)
    {
        var Q = new HashSet<Point>();
        var dist = new Dictionary<Point, int>();
        var prev = new Dictionary<Point, Point?>();

        foreach (var v in Graph.Keys)
        {
            dist[v] = int.MaxValue;
            prev[v] = null;
            Q.Add(v);
        }
        dist[source] = 0;

        while (Q.Count > 0)
        {
            var u = Q.MinBy(p => dist[p])!;

            Q.Remove(u);

            foreach (var v in u.Neighbors().Intersect(Q))
            {
                var alt = dist[u] + Graph[v];
                if (alt < dist[v])
                {
                    dist[v] = alt;
                    prev[v] = u;
                }
            }
        }

        var seq = new List<(Point, int, int)>();
        var p = target;
        while (p is not null)
        {
            seq.Add((p, Graph[p], dist[p]));
            p = prev[p];
        }

        return dist[target];
    }
}
