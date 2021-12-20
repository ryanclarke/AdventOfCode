namespace AoC2021;

public class Day19
{
    public static void Run()
    {
        var scanners = File
                .ReadAllText("../../../input/19.txt")
                .Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
                .Select((scanner, i) => new Scanner(Vector3.Zero(), scanner
                    .Split("\n", StringSplitOptions.RemoveEmptyEntries)
                    .Skip(1)
                    .Select(coord => coord
                        .Split(',')
                        .Select(a => int.Parse(a))
                        .ToArray()
                        .X(Vector3.FromArray))
                    .ToList()))
                .ToList();

        var ready = new Queue<int>(new List<int> { 0 });
        var unconnected = new Queue<int>(Enumerable.Range(1, scanners.Count - 1));

        while (ready.TryDequeue(out int i))
        {
            var remaining = new Queue<int>();
            while (unconnected.TryDequeue(out int j))
            {
                var offset = Vector3.Zero();
                var rotation = Rotations.FirstOrDefault(rot => 12 <=
                    scanners[j].Beacons
                        .Select(beacon => rot!(beacon))
                        .SelectMany(beacon =>
                            scanners[i].Beacons
                                .Select(b => b - beacon)
                                .ToList())
                        .GroupBy(v => v)
                        .MaxBy(g => g.Count())!
                        .T(g => offset = g.Key)
                        .Count(), null); ;
                if (rotation is not null)
                {
                    ready.Enqueue(j);

                    scanners[j] = new Scanner(offset, scanners[j].Beacons.Select(b => offset + rotation(b)).ToList());
                }
                else
                {
                    remaining.Enqueue(j);
                }
            }
            unconnected = remaining;
        }

        scanners.SelectMany(s => s.Beacons).Distinct().Count().Dump("19a (405): ");
        scanners.Select(s => s.Location).Max(a => scanners.Select(s => s.Location).Max(b => a.Manhattan(b))).Dump("19b (12306): ");
    }

    public record Scanner(Vector3 Location, List<Vector3> Beacons);

    public record Vector3(int X, int Y, int Z)
    {
        public int Manhattan(Vector3 v) => Math.Abs(X - v.X) + Math.Abs(Y - v.Y) + Math.Abs(Z - v.Z);

        public static Vector3 FromArray(int[] a) => new(a[0], a[1], a[2]);
        public static Vector3 Zero() => new(0, 0, 0);

        public static Vector3 operator +(Vector3 a, Vector3 b) => new(a.X + b.X, a.Y + b.Y, a.Z + b.Z);
        public static Vector3 operator -(Vector3 a, Vector3 b) => new(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    }

    public static List<Func<Vector3, Vector3>> Rotations = new()
    {
        (Vector3 v) => new(v.X, v.Y, v.Z),
        (Vector3 v) => new(v.Z, v.Y, -v.X),
        (Vector3 v) => new(-v.X, v.Y, -v.Z),
        (Vector3 v) => new(-v.Z, v.Y, v.X),
        (Vector3 v) => new(v.Y, -v.X, v.Z),
        (Vector3 v) => new(v.Z, -v.X, -v.Y),
        (Vector3 v) => new(-v.Y, -v.X, -v.Z),
        (Vector3 v) => new(-v.Z, -v.X, v.Y),
        (Vector3 v) => new(-v.X, -v.Y, v.Z),
        (Vector3 v) => new(v.Z, -v.Y, v.X),
        (Vector3 v) => new(v.X, -v.Y, -v.Z),
        (Vector3 v) => new(-v.Z, -v.Y, -v.X),
        (Vector3 v) => new(-v.X, -v.Z, -v.Y),
        (Vector3 v) => new(-v.Y, -v.Z, v.X),
        (Vector3 v) => new(v.X, -v.Z, v.Y),
        (Vector3 v) => new(v.Y, -v.Z, -v.X),
        (Vector3 v) => new(-v.Z, v.X, -v.Y),
        (Vector3 v) => new(-v.Y, v.X, v.Z),
        (Vector3 v) => new(v.Z, v.X, v.Y),
        (Vector3 v) => new(v.Y, v.X, -v.Z),
        (Vector3 v) => new(v.X, v.Z, -v.Y),
        (Vector3 v) => new(-v.Y, v.Z, -v.X),
        (Vector3 v) => new(-v.X, v.Z, v.Y),
        (Vector3 v) => new(v.Y, v.Z, v.X)
    };

}
