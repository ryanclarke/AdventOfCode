using System.Globalization;
using Point = (int Y, int x);

public class Day03
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/03.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();

    var symbols = new Dictionary<Point, char>();
    var partIds = new Dictionary<Point, Guid>();
    var partNumbers = new Dictionary<Guid, int>();
    var currentPartNumber = 0;
    var currentPartId = Guid.NewGuid();
    for (int y = 0; y < input.Count; y++)
    {
      for (int x = 0; x < input[y].Length; x++)
      {
        var c = input[y][x];
        if (int.TryParse($"{c}", out int i))
        {
          partIds.Add((y, x), currentPartId);
          currentPartNumber = currentPartNumber * 10 + i;
        }
        else
        {
          TryAddPartNumber();
          if (c != '.')
          {
            symbols.Add((y, x), c);
          }
        }
      }
      TryAddPartNumber();
    }

    symbols
      .Keys
      .SelectMany(point => Neighbors(point)
        .Where(n => partIds.ContainsKey(n))
        .Select(n => partIds[n]))
      .Distinct()
      .Sum(id => partNumbers[id])
      .Dump("03a [535078]: ");
    
    symbols
      .Where(p => p.Value == '*')
      .Select(p => p.Key)
      .Sum(point => Neighbors(point)
        .Where(n => partIds.ContainsKey(n))
        .Select(n => partIds[n])
        .Distinct()
        .Select(id => partNumbers[id])
        .X(n => n.Count() == 2 ? n.Multiply() : 0))
      .Dump("03b [75312571]: ");


    void TryAddPartNumber()
    {
      if (currentPartNumber > 0)
      {
        partNumbers.Add(currentPartId, currentPartNumber);
        currentPartId = Guid.NewGuid();
        currentPartNumber = 0;
      }
    }
    Point[] Neighbors(Point coord)
    {
      var (y, x) = coord;
      return new (int y, int x)[]
      {
        (y - 1, x - 1),
        (y,     x - 1),
        (y + 1, x - 1),
        (y - 1, x),
        (y + 1, x),
        (y - 1, x + 1),
        (y,     x + 1),
        (y + 1, x + 1)
      };
    }
  }
}
