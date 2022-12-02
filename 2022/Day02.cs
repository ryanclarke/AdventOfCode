namespace AoC2022;

public class Day02
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/02.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        input.Sum(line => line[2] - 87 + (2 + line[2] - line[0]) % 3 * 3).Dump("02a (11603): ");
        input.Sum(line => (2 + line[0] + line[2]) % 3 + 1 + ((line[2] - 88) * 3)).Dump("02b (12725): ");
    }
}
