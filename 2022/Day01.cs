using static AoC2022.Utils;

namespace AoC2022;

public class Day01
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/01.txt";
    public static void Run()
    {
        PatternMatchingAggregator();
        ForeachLoop();
        SplitOnNewLines();
    }

    private static void PatternMatchingAggregator()
    {
        File
            .ReadAllLines(InputFilePath)
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
            .OrderByDescending(ID)
            .ToArray()
            .T(elves => elves.Take(1).Sum().Dump("01a (72017): "))
            .T(elves => elves.Take(3).Sum().Dump("01b (212520): "));
    }

    private static void ForeachLoop()
    {
        var input = File.ReadAllLines(InputFilePath);
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

        elves = elves.OrderByDescending(ID).ToList();
        elves.Take(1).Sum().Dump("01a (72017): ");
        elves.Take(3).Sum().Dump("01b (212520): ");
    }

    private static void SplitOnNewLines()
    {
        var input = File
            .ReadAllText(InputFilePath)
            .Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
            .Select(elf => elf
                .Split("\n", StringSplitOptions.RemoveEmptyEntries)
                .Select(int.Parse)
                .Sum())
            .OrderByDescending(ID)
            .ToList();

        input.First().Dump("01a (72017): ");
        input.Take(3).Sum().Dump("01b (212520): ");
    }
}
