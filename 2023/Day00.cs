public class Day00
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/00.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
  }
}
