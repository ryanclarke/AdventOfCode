public class Day07
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/07.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
    
    var hands = input
      .Select(line => line.Split(' ')
        .X(a => new Hand(
          Cards: [.. a[0]],
          Bid: int.Parse(a[1])
        )))
      .ToList();

    hands
      .OrderBy(h => h.HandType(jokers: false))
      .ThenBy(h => h.Cards, new HighCardComparer(jokers: false))
      .Select((h, i) => h.Bid * (i + 1))
      .Sum()
      .Dump("07a [248569531]: ");

    hands
      .OrderBy(h => h.HandType(jokers: true))
      .ThenBy(h => h.Cards, new HighCardComparer(jokers: true))
      .Select((h, i) => h.Bid * (i + 1))
      .Sum()
      .Dump("07b [250382098]: ");
  }

  public record Hand(char[] Cards, int Bid)
  {
    public int[] HandStructure(bool jokers) => Cards
      .Where(c => !jokers || c != 'J')
      .GroupBy(Utils.ID)
      .Select(x => x.Count())
      .OrderByDescending(Utils.ID)
      .ToArray()
      .X(a => a.Length == 0
        ? [Cards.Count(i => jokers && i == 'J')]
        : a
          .Select((x, i) => i == 0 ? x + Cards.Count(i => jokers && i == 'J') : x)
          .ToArray());
    public int HandType(bool jokers) => HandStructure(jokers) switch
    {
      [5] => 7,
      [4, 1] => 6,
      [3, 2] => 5,
      [3, 1, 1] => 4,
      [2, 2, 1] => 3,
      [2, 1, 1, 1] => 2,
      [1, 1, 1, 1, 1] => 1,
      _ => throw new NotSupportedException()
    };
  };

  public static Func<char, int> FaceToValue(bool jokers) => (char c) => c switch
  {
    'A' => 14,
    'K' => 13,
    'Q' => 12,
    'J' => jokers ? 0 : 11,
    'T' => 10,
    _ => int.Parse($"{c}")
  };

  public class HighCardComparer(bool jokers) : IComparer<char[]>
  {
    private readonly Func<char, int> ftv = FaceToValue(jokers);
    public int Compare(char[]? x, char[]? y)
    {
      if (x is null || y is null) return 0;
      for (int i = 0; i < x.Length; i++)
      {
        var (a, b) = (ftv(x[i]), ftv(y[i]));
        if (a > b) return 1;
        if (a < b) return -1;
      }
      return 0;
    }
  }
}