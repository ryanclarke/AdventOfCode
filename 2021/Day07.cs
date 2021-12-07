namespace AoC2021;

public class Day07
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/07.txt")
                .First()
                .Split(',')
                .Select(int.Parse)
                .ToList();

        Enumerable.Range(0, input.Max())
               .Min(x => input.Sum(i => Math.Abs(i - x)))
               .Dump("7a (325528): ");

        Enumerable.Range(0, input.Max())
               .Min(x => input.Sum(i => Math.Abs(i - x).λ(a => (Math.Pow(a, 2) + a) / 2)))
               .Dump("7b (85015836): ");
    }
}
