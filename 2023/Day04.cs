public partial class Day04
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/04.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
    
    var cards = input
      .Select(l => MultipleWhitespace().Replace(l, " ").Split(':', '|')
        .X(a => a[1].Arrayify().Intersect(a[2].Arrayify()).Count()))
      .ToList();

    cards
      .Where(c => c > 0)
      .Select(c => Math.Pow(2, c - 1))
      .Sum()
      .Dump("04a [21485]: ");

    cards
      .Reverse<int>()
      .Aggregate(Enumerable.Empty<int>(),
        (a, c) => a.Append(1 + a.TakeLast(c).Sum()))
      .Sum()
      .Dump("04b [11024379]: ");
  }

  [GeneratedRegex(" +")]
  private static partial Regex MultipleWhitespace();
}
