namespace AoC2021;

public class Day06
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/06.txt")
                .First()
                .Split(',')
                .Select(int.Parse)
                .ToList();

        var catalogue = input.GroupBy(x => x).ToDictionary(x => x.Key, x => x.Count());
        var fishes = Enumerable.Range(0, 9).Select(i => catalogue.ContainsKey(i) ? catalogue[i] : 0L).ToArray();
        for (int i = 0; i < 256; i++)
        {
            if (i == 80)
            {
                fishes.Sum().Dump("6a (358214): ");
            }
            var births = fishes[0];
            fishes = fishes.Skip(1).Append(births).ToArray();
            fishes[6] += births;
        }
        fishes.Sum().Dump("6b (1622533344325): ");
    }
}
