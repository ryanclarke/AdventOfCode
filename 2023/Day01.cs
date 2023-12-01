public partial class Day01
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/01.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
    input
      .Select(s => LowercaseRegex().Replace(s, ""))
      .Select(s => int.Parse($"{s.First()}") * 10 + int.Parse($"{s.Last()}"))
      .Sum()
      .Dump("01a [53334]: ");

      input
      .Select(s => s
          .Replace("one", "o1e")
          .Replace("two", "t2o")
          .Replace("three", "t3e")
          .Replace("four", "f4r")
          .Replace("five", "f5e")
          .Replace("six", "s6x")
          .Replace("seven", "s7n")
          .Replace("eight", "e8t")
          .Replace("nine", "n9e")
      )
      .Select(s => LowercaseRegex().Replace(s, ""))
      .Select(s => int.Parse($"{s.First()}") * 10 + int.Parse($"{s.Last()}"))
      .Sum()
      .Dump("01b [52834]: ");
  }

  [GeneratedRegex("[a-z]")]
  private static partial Regex LowercaseRegex();
}
