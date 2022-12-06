namespace AoC2022;

public class Day06
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/06.txt";
    public static void Run()
    {
        var input = new ArraySegment<char>(File.ReadLines(InputFilePath).First().ToCharArray());

        FindMarker(4).Dump("06a (1623): ");
        FindMarker(14).Dump("06b (3774): ");

        int FindMarker(int length)
        {
            for (int i = length; i < input.Count; i++)
            {
                if (input.Slice(i-length, length).Distinct().Count() == length)
                {
                    return i;
                }
            }
            throw new Exception("Not Found");
        }
    }
}
