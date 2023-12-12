using Galaxy = (int Y, int X);

public class Day11
{
  public static List<int> LargeRows = [];
  public static List<int> LargeCols = [];
  private static readonly string InputFilePath = $"{Config.InputRoot}/11.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
    
    for (int i = input[0].Length - 1; i >=0; i--)
    {
      if (input.All(line => line[i] == '.'))
      {
        LargeCols.Add(i);
      }
    }
    LargeRows = input
      .Select((line, i) => line.All(c => c =='.') ? i : -1)
      .Where(i => i > -1)
      .ToList();

    Galaxy[] galaxies = input
      .SelectMany((line, y) => line
        .Select((c, x) => c == '#' ? (Y: y, X: x) : (Y: -1, X: -1)))
      .Where(p => p != (Y: -1, X: -1))
      .ToArray();

    var all = 0L;
    for (int i = 0; i < galaxies.Length-1; i++)
    {
      var a = galaxies[i];
      foreach (var b in galaxies.Skip(i))
      {
        all += Dist(2, a, b);
      }
    }
    all.Dump("11a [10231178]: ");

    all = 0;
    for (int i = 0; i < galaxies.Length-1; i++)
    {
      var a = galaxies[i];
      foreach (var b in galaxies.Skip(i))
      {
        all += Dist(1_000_000, a, b);
      }
    }
    all.Dump("11b [622120986954]: ");

    int yy = 12;
  }

  public static long Dist(int multiple, Galaxy a, Galaxy b) {
    var (rmax, rmin) = a.Y > b.Y ? (a.Y, b.Y) : (b.Y, a.Y);
    var (cmax, cmin) = a.X > b.X ? (a.X, b.X) : (b.X, a.X);
    var cs = LargeCols.Where(i => i < cmax && i > cmin).Count();
    var rs = LargeRows.Where(i => i < rmax && i > rmin).Count();
    var x = multiple - 1;
    
    return Math.Abs(rmax - rmin) + Math.Abs(cmax - cmin) + (cs * x) + (rs * x);
  }
}
