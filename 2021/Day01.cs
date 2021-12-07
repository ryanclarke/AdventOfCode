namespace AoC2021;

public class Day01
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/01.txt")
                .Select(int.Parse)
                .ToList();

        Enumerable.Zip(
                input.Take(..^1), 
                input.Take(1..))
            .Select(t => t.Second > t.First)
            .Count(x => x)
            .Dump("1a: ");

        var sums = Enumerable.Zip(
                input.Take(..^2),
                input.Take(1..^1),
                input.Take(2..))
            .Select(t => t.First + t.Second + t.Third)
            .ToList();
        Enumerable.Zip(
                sums.Take(..^1),
                sums.Take(1..))
            .Select(t => t.Second > t.First)
            .Count(x => x)
            .Dump("1b: ");
    }
}
