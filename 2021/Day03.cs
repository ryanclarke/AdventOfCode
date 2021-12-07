namespace AoC2021;

public class Day03
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/03.txt")
                .ToList();

        var gamma = Enumerable.Range(0, 12)
            .Select(i => input.Count(n => n[i] == '1') > 500 ? "1" : "0")
            .JoinStrings()
            .λ(s => Convert.ToInt32(s, 2));
        var epsilon = (int)Math.Pow(2, 12) - 1 - gamma;

        (gamma * epsilon).Dump("3a (2648450): ");


        var oxygen = FindRatingNumber(input.ToList(), 0, (a, b) => a >= b ? '1' : '0');
        var co2 = FindRatingNumber(input.ToList(), 0, (a, b) => a < b ? '1' : '0');

        (oxygen * co2).Dump("3b (2845944): ");
    }

    private static int FindRatingNumber(List<string> list, int index, Func<int, int, char> compare)
    {
        if (list.Count == 1)
        {
            return list.Single().λ(s => Convert.ToInt32(s, 2));
        }
        var b = compare(list.Count(n => n[index] == '1'), list.Count(n => n[index] == '0'));
        return FindRatingNumber(list.Where(n => n[index] == b).ToList(), index + 1, compare);
    }
}
