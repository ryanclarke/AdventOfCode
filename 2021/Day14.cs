namespace AoC2021;

public static class Day14
{
    public static Dictionary<(char, char), char> PairInsertionRules { get; private set; } = new Dictionary<(char, char), char>();

    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/14.txt")
                .ToList();

        PairInsertionRules = input.Skip(2)
            .ToDictionary(a => (a[0], a[1]), a => a.Last());

        PolymerResult(input.First().ToList(), 10)
            .PolymerCounts()
            .Select(t => t.Count)
            .X(counts => counts.Max() - counts.Min())
            .Dump("14a (3555): ");

        var ruleCounts = PairInsertionRules.ToDictionary(r => r.Key, r => PolymerResult(r.Key, 21).PolymerCounts(true));
        var intermediate = PolymerResult(input.First().ToList(), 19);
        var counts = new List<(char Element, double Count)>();
        for (int i = 0; i < intermediate.Count - 1; i++)
        {
            counts.AddRange(ruleCounts[(intermediate[i], intermediate[i + 1])]);
        }
        counts.Add((input.First().Last(), 1L));

        var list = counts
            .GroupBy(t => t.Element)
            .Select(g => g.Sum(t => t.Count))
            .X(counts => counts.Max() - counts.Min())
            .Dump("14b (4439442043739): ");
    }
    public static List<char> PolymerResult(this (char E1, char E2) rule, int steps) =>
        PolymerResult(new List<char> { rule.E1, rule.E2 }, steps);

    public static List<char> PolymerResult(this List<char> polymer, int steps)
    {
        for (int step = 1; step <= steps; step++)
        {
            var next = new List<char>();
            for (int i = 0; i < polymer.Count - 1; i++)
            {
                next.Add(polymer[i]);
                next.Add(PairInsertionRules[(polymer[i], polymer[i + 1])]);
            }
            next.Add(polymer.Last());
            polymer = next;
        }
        return polymer;
    }

    public static List<(char Element, double Count)> PolymerCounts(this List<char> polymer, bool removeLast = false) =>
        polymer
            .X(p => removeLast ? p.Take(p.Count - 1) : p)
            .GroupBy(c => c)
            .Select(g => (g.Key, (double) g.LongCount()))
            .ToList();
}
