namespace AoC2022;

public static class Day03
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/03.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        input
            .Sum(rucksack => rucksack
                .Take(rucksack.Length / 2)
                .Intersect(rucksack
                    .TakeLast(rucksack.Length / 2))
                .Single()
                .Prioritize())
            .Dump("03a (8105): ");
        
        input
            .Chunk(3)
            .Sum(group => group[0]
                .Intersect(group[1])
                .Intersect(group[2])
                .Single()
                .Prioritize())
            .Dump("03b (2363): ");
    }
    
    private static int Prioritize(this char c) =>
        (int) c switch
        {
            > 96 => c - 96,
            _ => c - 38
        };
}
