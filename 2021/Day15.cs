namespace AoC2021;

public static class Day15
{

    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/15.txt")
                .ToList();

        var data = input
                .SelectMany((line, y) => line.Select((c, x) => (Coord: new Point(x, y), Risk: int.Parse(c.ToString()))))
                .ToList();

        var width = input.First().Length;
        var height = input.Count;
        data.ToDictionary(t => t.Coord, t => t.Risk)
            .Search(new Point(0, 0), new Point(width - 1, height - 1))
            .Dump("15a (373): ");

        var expand = 5;
        var offsets = Enumerable.Range(0, expand)
                    .SelectMany(x => Enumerable.Range(0, expand).Select(y => new Point(x, y)))
                    .ToList();
        var tuples = offsets
            .SelectMany(offset => data
                .Select(t => (
                    Coord: new Point(t.Coord.X + (offset.X * width), 
                                     t.Coord.Y + (offset.Y * height)), 
                    Risk: ((t.Risk + offset.X + offset.Y) % 9).X(i => i == 0 ? 9 : i))))
            .ToList();
        var graph = tuples
                    .ToDictionary(t => t.Coord, t => t.Risk);
        graph
            .Search(new Point(0, 0), new Point(width * expand - 1, height * expand - 1))
            .Dump("15b (2868): ");
    }

    public record Point(int X, int Y);

    public static HashSet<Point> Neighbors(this Point point, Point Max)
    {
        var (x, y) = (point.X, point.Y);
        return new Point[]
        {
                new Point(x,     y - 1),
                new Point(x - 1, y),
                new Point(x + 1, y),
                new Point(x,     y + 1)
        }
        .Where(p => p.X >= 0 && p.X <= Max.X && p.Y >= 0 && p.Y <= Max.Y)
        .ToHashSet();
    }

    public static int Search(this Dictionary<Point, int> graph, Point source, Point target)
    {
        var risks = graph.ToDictionary(p => p.Key, _ => int.MaxValue);
        var Q = new Queue<Point>();
        Q.Enqueue(source);
        risks[source] = 0;
        while (Q.Count > 0)
        {
            var current = Q.Dequeue();
            foreach (var neighbor in current.Neighbors(target))
            {
                var risk = risks[current] + graph[neighbor];
                if (risks[neighbor] > risk)
                {
                    risks[neighbor] = risk;
                    Q.Enqueue(neighbor);
                }
            }
        }

        return risks[target];
    }

    public static int Dijkstra(this Dictionary<Point, int> graph, Point source, Point target)
    {
        var Q = new HashSet<Point>();
        var dist = new Dictionary<Point, int>();
        var prev = new Dictionary<Point, Point?>();

        foreach (var v in graph.Keys)
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

            foreach (var v in u.Neighbors(target).Intersect(Q))
            {
                var alt = dist[u] + graph[v];
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
            seq.Add((p, graph[p], dist[p]));
            p = prev[p];
        }

        return dist[target];
    }
}
