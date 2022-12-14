namespace AoC2022;

public class Day10
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/10.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var cycle = 0;
        var registerX = 1;
        var positions = new int[240];
        foreach (var instruction in input)
        {
            switch (instruction.Split())
            {
                case ["addx", var val]:
                    positions[cycle++] = registerX;
                    positions[cycle++] = registerX;
                    registerX += int.Parse(val);
                    break;
                case ["noop"]:
                    positions[cycle++] = registerX;
                    break;
            }
        }

        new[] {20, 60, 100, 140, 180, 220}.Sum(i => i * positions[i - 1]).Dump("10a (15680): ");

        positions
            .Select((val, i) => new[] {val, val + 1, val + 2}.Contains((i + 1) % 40) ? '█' : ' ')
            .Chunk(40)
            .Select(c => c.Stringify())
            .JoinLines()
            .Dump($"10b (ZFBFHGUP):\n");
    }
}
