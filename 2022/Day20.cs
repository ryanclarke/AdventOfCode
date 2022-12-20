using System.Numerics;

namespace AoC2022;

public class Day20
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/20.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        Mix(1, 1).Dump("20a (11123): ");
        Mix(811589153, 10).Dump("20b (4248669215955): ");

        BigInteger Mix(BigInteger decryptionKey, int mixCount)
        {
            var pairs = input
                .Select(BigInteger.Parse)
                .Select((value, start) => (start, value: value * decryptionKey))
                .ToList();

            for (int i = 0; i < pairs.Count * mixCount; i++)
            {
                var index = pairs.FindIndex(p => p.start == (i % pairs.Count));
                var element = pairs[index];
                pairs.RemoveAt(index);
                var move = (int)(element.value % pairs.Count);
                pairs.Insert((10 * pairs.Count + index + move) % pairs.Count, element);
            }

            var zero = pairs.FindIndex(p => p.value == 0);
            return new[] { 1000, 2000, 3000 }
                .Select(i => pairs.ElementAt((zero + i) % pairs.Count).value)
                .Aggregate(BigInteger.Zero, (acc, bi) => acc + bi);
        }
    }
}
