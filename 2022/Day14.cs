namespace AoC2022;

public class Day14
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/14.txt";
    private static readonly char Rock = '█';
    private static readonly char Sand = '◯';
    private static readonly char Air = ' ';
    
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .Select(line => line
                    .Split(" -> ")
                    .Select(pair => pair
                        .Split(',')
                        .X(a => new YX(int.Parse(a[1]), int.Parse(a[0]))))
                    .ToList())
                .ToList();

        var rocks = new HashSet<YX>();
        foreach (var wall in input)
        {
            var queue = new Queue<YX>(wall);
            var last = queue.Dequeue();
            rocks.Add(last);
            while (queue.Any())
            {
                var dest = queue.Peek();
                var next = last.MoveTo(dest);
                rocks.Add(next);
                last = next;
                if (last == dest)
                {
                    queue.Dequeue();
                }
            }
        }
        
        var floor = 2 + rocks.Select(r => r.Y).Max();

        var sands = new HashSet<YX>();
        Simulate();
        sands.Count.Dump("14a (1330): ");

        rocks.UnionWith(Enumerable.Range(0, 1000).Select(x => new YX(floor, x)));
        Simulate();
        sands.Count.Dump("14b (26139): ");

        void Simulate()
        {
            var sand = YX.SandSpawn;
            while (sand.Y <= floor + 1 && !sands.Contains(YX.SandSpawn))
            {
                var next = sand
                    .FallOptions()
                    .FirstOrDefault(s => !rocks.Contains(s) && !sands.Contains(s));
                if (next is null)
                {
                    sands.Add(sand);
                    sand = YX.SandSpawn;
                }
                else
                {
                    sand = next;
                }
            }
        }
    }

    private record YX(int Y, int X)
    {
        public static readonly YX SandSpawn = new(0, 500);
        public static readonly YX Zero = new(0, 0);
        public static readonly YX One = new(1, 1);
        public static readonly YX Down = new(1, 0);
        public static readonly YX DownLeft = new(1, -1);
        public static readonly YX DownRight = new(1, 1);
        
        public static YX operator +(YX a, YX b) => new(a.Y + b.Y, a.X + b.X);
        public static YX operator -(YX a, YX b) => new(a.Y - b.Y, a.X - b.X);
        public static YX operator *(YX a, YX b) => new(a.Y * b.Y, a.X * b.X);
        public static YX operator /(YX a, YX b) => new(a.Y / b.Y, a.X / b.X);

        public YX[] FallOptions() => new []
        {
            this + Down,
            this + DownLeft,
            this + DownRight,
        };
        
        public YX CompareTo(YX b) => new(Y.CompareTo(b.Y), X.CompareTo(b.X));

        public YX MoveTo(YX dest) => this - CompareTo(dest) * One;
    };
}
