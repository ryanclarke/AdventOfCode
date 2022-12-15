using System.Numerics;
using System.Text.RegularExpressions;

namespace AoC2022;

public class Day15
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/15.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var sensors = input
            .Select(line => Regex
                .Matches(line, "([-0-9]+)")
                .X(m => new Sensor(new YX(intIt(m[1]),intIt(m[0])), new YX(intIt(m[3]),intIt(m[2])))))
            .ToList();

        sensors
            .SelectMany(sensor =>
            {
                var distanceFromBeacon = sensor.Manhattan();
                var distanceFromRow = Math.Abs(sensor.Location.Y - 2_000_000);
                var remainder = distanceFromBeacon - distanceFromRow;
                return remainder <= 0 
                    ? Enumerable.Empty<YX>() 
                    : Enumerable
                        .Range(sensor.Location.X - remainder, remainder * 2 + 1)
                        .Select(x => new YX(2_000_000, x));
            })
            .Distinct()
            .Except(sensors.Select(sensor => sensor.Beacon))
            .Count()
            .Dump("15a (5564017): ");

        for (var y = 0; y < 4_000_000; y++)
        {
            for (var x = 0; x < 4_000_000; x++)
            {
                var sensor = sensors.FirstOrDefault(sensor => sensor.Manhattan(new YX(y, x)) <= sensor.Manhattan());
                if (sensor is null)
                {
                    (x * (BigInteger)4_000_000 + y).Dump($"15b (11558423398893): ");
                    break;
                }
                else
                {
                    var distanceFromBeacon = sensor.Manhattan();
                    var distanceFromRow = Math.Abs(sensor.Location.Y - y);
                    var remainder = distanceFromBeacon - distanceFromRow;
                    x = sensor.Location.X + remainder;
                }
            }
        }
            

        int m = 12;
    }

    private static int intIt(Capture m) => int.Parse(m.Value);

    private record Sensor(YX Location, YX Beacon)
    {
        public int Manhattan() => Manhattan(Beacon);
        public int Manhattan(YX dest) => (Location - dest).Manhattan();
    };
    
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
        
        public int Manhattan() => Math.Abs(Y) + Math.Abs(X);
    };
}
