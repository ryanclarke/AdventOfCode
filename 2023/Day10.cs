using Point = (int Y, int X);

public class Day10
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/10.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();

    var tiles = input
      .SelectMany((line, y) => line
        .Select((c, x) => (Point: (Y: y, X: x), Neighbors: c switch
        {
          '|' => [(Y: y - 1, X: x), (Y: y + 1, X: x)],
          '-' => [(Y: y, X: x - 1), (Y: y, X: x + 1)],
          'L' => [(Y: y - 1, X: x), (Y: y, X: x + 1)],
          'J' => [(Y: y - 1, X: x), (Y: y, X: x - 1)],
          '7' => [(Y: y + 1, X: x), (Y: y, X: x - 1)],
          'F' => [(Y: y + 1, X: x), (Y: y, X: x + 1)],
          'S' => [(Y: y - 1, X: x), (Y: y + 1, X: x), (Y: y, X: x - 1), (Y: y, X: x + 1)],
          _ => Array.Empty<Point>()
        })))
      .ToDictionary();

    var start = tiles.Single(kvp => kvp.Value.Length == 4).Key;
    tiles[start] = tiles.ToList().Where(a => a.Value.Contains(start)).Select(a => a.Key).ToArray();

    List<Point> path = [tiles[start].First(), start];
    while (true)
    {
      var next = tiles[path[0]].Single(a => a != path[1]);
      if (next == start)
      {
        break;
      }
      path.Insert(0, next);
    }
    (path.Count / 2).Dump("10a [6927]: ");

    var pathSet = path.ToHashSet();

    var insides = 0;
    for (int y = 0; y < input.Count; y++)
    {
      var line = "";
      for (int x = 0; x < input[y].Length; x++)
      {
        if (pathSet.Contains((y, x)))
        {
          line += input[y][x] == '|' ? 'l' : input[y][x];
        }
        else
        {
          var count = Regex.Matches(line, "(l|F-*J|F-*S|L-*7)").Count;
          if (count % 2 == 1)
          {
            insides++;
          }
        }
      }
    }
    insides.Dump("10b [467]: ");
  }
}
