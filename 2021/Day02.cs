namespace AoC2021;

public class Day02
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/02.txt")
                .Select(s => s.Split(' ').λ(ss => new Command(Enum.Parse<Direction>(ss[0]), ss[1].λ(int.Parse))))
                .ToList();

        input.Select(c => c.Direction switch
            {
                Direction.forward => (c.Distance, 0),
                Direction.up => (0, -c.Distance),
                Direction.down => (0, c.Distance),
                _ => (0, 0)
            })
            .Aggregate((0, 0), (a, t) => (a.Item1 + t.Item1, a.Item2 + t.Item2), a => a.Item1 * a.Item2)
            .Dump("2a: ");


        input.Aggregate(new State(0, 0, 0), (s, c) => c.Direction switch
        {
            Direction.forward => s with { H = s.H + c.Distance, D = s.D + (s.Aim * c.Distance)},
            Direction.up => s with { Aim = s.Aim - c.Distance},
            Direction.down => s with { Aim = s.Aim + c.Distance },
            _ => s
        }, s => s.H * s.D)
        .Dump("2b: ");
    }

    private enum Direction
    {
        forward,
        up,
        down
    }
    
    private record Command(Direction Direction, int Distance);
    private record State(int H, int D, int Aim);
}
