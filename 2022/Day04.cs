using static System.Linq.Enumerable;

namespace AoC2022;

public class Day04
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/04.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        // First way
        input.Count(pair => pair
                .Split('-', ',')
                .Select(int.Parse)
                .ToArray() switch
                {
                    [var a, var b, var c, var d] when a <= c && b >= d => true,
                    [var a, var b, var c, var d] when c <= a && d >= b => true,
                    _ => false
                }
        ).Dump("04a (453): ");
        
        input.Count(pair => pair
                .Split('-', ',')
                .Select(int.Parse)
                .ToArray()
                .X(sectionIds => 
                    sectionIds is [var a1, var a2, var b1, var b2] 
                    && Range(a1, a2).Intersect(Range(b1, b2)).Any())
        ).Dump("04b (919): ");
        
        // Second way
        var sectionIds = input.Select(pair => pair
                .Split('-', ',')
                .Select(int.Parse)
                .ToArray()
                .X(sectionIds => 
                    sectionIds is [var a1, var a2, var b1, var b2] 
                        ? new Pair(new Area(a1, a2), new Area(b1, b2))
                        : null
                ))
            .Cast<Pair>()
            .ToList();
        
        sectionIds.Count(pair =>
                pair.A.Start <= pair.B.Start && pair.A.End >= pair.B.End 
             || pair.B.Start <= pair.A.Start && pair.B.End >= pair.A.End
        ).Dump("04a (453): ");
        
        sectionIds
            .Count(pair => pair.A.Enumerable.Intersect(pair.B.Enumerable).Any())
            .Dump("04b (919): ");
    }

    private record Area(int Start, int End)
    {
        public IEnumerable<int> Enumerable => Range(Start, End);
    };
    private record Pair(Area A, Area B);
}
