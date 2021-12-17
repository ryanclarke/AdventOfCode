namespace AoC2021;

public class Day17
{
    public static void Run()
    {
        var targetX = (start: 248, end: 285);
        var targetY = (start: -85, end: -56);
        //targetX = (start: 20, end: 30);
        //targetY = (start: -10, end: -5);

        Enumerable.Range(0, Math.Abs(targetY.start)).Sum().Dump("17a (3570): ");

        var velocities = new HashSet<(int,int)>();
        for (int y = targetY.start; y <= Math.Abs(targetY.start); y++)
        {
            for (int x = 0; x <= targetX.end; x++)
            {
                var ys = Enumerable.Range(0, Math.Abs(targetY.start) * 2 + 1)
                    .Aggregate(new List<int>(), 
                            (l, i) => l.Append(l.LastOrDefault() + y - i).ToList())
                    .ToList();
                var xs = Enumerable.Range(0, Math.Abs(targetY.start) * 2 + 1)
                    .Aggregate(new List<int>(),
                            (l, i) => l.Append(l.LastOrDefault() + Math.Max(0, x - i)).ToList())
                    .ToList();
                if (xs.Zip(ys).Any(p => p.First >= targetX.start && p.First <= targetX.end && p.Second >= targetY.start && p.Second <= targetY.end))
                {
                    velocities.Add((x, y));
                }
            }
        }
        velocities.Count.Dump("17b (): ");
    }
}
