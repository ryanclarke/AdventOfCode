namespace AoC2021;

public class Day22
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/22.txt")
                .ToList();

        var cuboids = input.Select(ParseLine).Reverse().ToList();
        var region = Enumerable.Range(-50, 101);
        var zRules = region.ToDictionary(a => a, a => cuboids.Select((c, i) => c.Cuboid.Z.Has(a) ? i : -1).Where(i => i > 0).ToHashSet());
        var yRules = region.ToDictionary(a => a, a => cuboids.Select((c, i) => c.Cuboid.Y.Has(a) ? i : -1).Where(i => i > 0).ToHashSet());
        var xRules = region.ToDictionary(a => a, a => cuboids.Select((c, i) => c.Cuboid.X.Has(a) ? i : -1).Where(i => i > 0).ToHashSet());

        long onCubes = 0;
        foreach (var zRule in zRules)
        {
            foreach (var yRule in yRules)
            {
                var validRules = zRule.Value.Intersect(yRule.Value).ToHashSet();
                if (validRules.Any())
                {
                    foreach (var xRule in xRules)
                    {
                        var rule = validRules.Intersect(xRule.Value).FirstOrDefault(-1);
                        if (rule > -1 && cuboids[rule].State)
                        {
                            onCubes++;
                        }
                    }
                }
            }
        }
        onCubes.Dump("22a (570915): ");

        var resultingCubes = new List<(bool State, Cuboid Cuboid)>();
        foreach (var cube in input.Select(ParseLine).ToList())
        {
            var cubesToAdd = new List<(bool State, Cuboid Cuboid)>();
            if (cube.State)
            {
                cubesToAdd.Add(cube);
            }
            foreach (var otherCube in resultingCubes)
            {
                var intersection = cube.Cuboid.Intersect(otherCube.Cuboid);
                if (intersection.Valid())
                {
                    cubesToAdd.Add((!otherCube.State, intersection));
                }
            }
            resultingCubes.AddRange(cubesToAdd);
        }
        resultingCubes.Sum(c => c.Cuboid.Volume() * (c.State ? 1 : -1)).Dump("22b (1268313839428137): ");
    }

    public record MinMax(int Min, int Max)
    {
        public bool Has(int a) => a >= Min && a <= Max;
        public long Size() => Max - Min + 1;

        public MinMax Intersect(MinMax a) => new MinMax(Math.Max(Min, a.Min), Math.Min(Max, a.Max)).X(m => m.Max < m.Min ? Invalid : m);
        //public long Overlap(MinMax a) => (Math.Min(Max, a.Max) - Math.Max(Min, a.Min)).X(l => l < 0 ? 0 : l + 1);

        public bool Valid() => Max >= Min;
        private static MinMax Invalid = new (int.MaxValue, int.MinValue);
    }

    public record Point3(int X, int Y, int Z)
    {
        public static bool operator <=(Point3 a, Point3 b) => a.X <= b.X && a.Y <= b.Y && a.Z <= b.Z;
        public static bool operator >=(Point3 a, Point3 b) => a.X >= b.X && a.Y >= b.Y && a.Z >= b.Z;
    }

    public record Cuboid(MinMax X, MinMax Y, MinMax Z)
    {
        public static Cuboid FromEnumerable(IEnumerable<MinMax> minMaxes) => minMaxes.ToArray().X(a => new Cuboid(a[0], a[1], a[2]));
        public long Volume() => X.Size() * Y.Size() * Z.Size();
        public Cuboid Intersect(Cuboid c) => new Cuboid(X.Intersect(c.X), Y.Intersect(c.Y), Z.Intersect(c.Z));
        public bool Valid() => X.Valid() && Y.Valid() && Z.Valid();
    }

    private static MinMax ParseRange(string range) => range.Split('=')[1].Split("..").X(a => new MinMax(int.Parse(a[0]), int.Parse(a[1])));
    private static (bool State, Cuboid Cuboid) ParseLine(string line) => line.Split(' ').X(a => (a[0] == "on", a[1].Split(',').Select(ParseRange).X(x => Cuboid.FromEnumerable(x))));
}
