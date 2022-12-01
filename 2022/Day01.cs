namespace AoC2022;

public class Day01
{
    public static void Run()
    {
        PatternMatchingAggregater();
        ForeachLoop();
        SplitOnNewLines();
    }

    private static void PatternMatchingAggregater()
    {
        File
            .ReadAllLines("../../../input/01.txt")
            .Select(line => line switch
            {
                "" => 0,
                _ => int.Parse(line)
            })
            .Aggregate(new List<int> {0}, (elves, calories) =>
            {
                return calories switch
                {
                    0 => elves.Prepend(0).ToList(),
                    _ => AddToFirst(elves, calories)
                };

                List<int> AddToFirst(List<int> elves, int i)
                {
                    elves[0] += i;
                    return elves;
                }
            })
            .OrderByDescending(Extensions.ID)
            .ToArray()
            .T(elves => elves.Take(1).Sum().Dump("01a (72017): "))
            .T(elves => elves.Take(3).Sum().Dump("01b (212520): "));
    }

    private static void ForeachLoop()
    {
        var input = File.ReadAllLines("../../../input/01.txt");
        var calories = 0;
        var elves = new List<int>();
        foreach (var item in input)
        {
            if (string.IsNullOrWhiteSpace(item))
            {
                elves.Add(calories);
                calories = 0;
            }
            else
            {
                calories += int.Parse(item);
            }
        }

        elves = elves.OrderByDescending(x => x).ToList();
        elves.Take(1).Sum().Dump("01a (72017): ");
        elves.Take(3).Sum().Dump("01b (212520): ");
    }

    private static void SplitOnNewLines()
    {
        var input = File
            .ReadAllText("../../../input/01.txt")
            .Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
            .Select(elf => elf
                .Split("\n", StringSplitOptions.RemoveEmptyEntries)
                .Select(int.Parse)
                .Sum())
            .OrderByDescending(x => x)
            .ToList();

        input.First().Dump("01a (72017): ");
        input.Take(3).Sum().Dump("01b (212520): ");
    }
}
