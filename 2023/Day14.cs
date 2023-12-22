using Point = (int y, int x);

public class Day14
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/14test.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
    
    var startingSpheres = input
      .SelectMany((line, y) => line
        .Select((c, x) => c == 'O' ? (y, x) : (-1,-1)))
      .Where(t => t != (-1,-1))
      .ToHashSet<Point>();
    var blocks = input
      .SelectMany((line, y) => line
        .Select((c, x) => c == '#' ? (y, x) : (-1,-1)))
      .Where(t => t != (-1,-1))
      .ToHashSet<Point>();
    var spin = new List<Func<Point, Point>>{
      s => (y: s.y - 1, x: s.x),
      s => (y: s.y, x: s.x - 1),
      s => (y: s.y + 1, x: s.x),
      s => (y: s.y, x: s.x + 1)
    };

    List<Point> spheres = [.. startingSpheres];
    List<Point> newSpheres = [.. startingSpheres];
    while(true)
    {
      newSpheres = spheres
        .Select(s => spin[0](s)
          .X(c => c.y < 0 || spheres.Contains(c) || blocks.Contains(c) ? s : c))
        .ToList();
      
      if (Enumerable.Range(0, spheres.Count).All(i => newSpheres[i] == spheres[i]))
      {
        break;
      }
      spheres = newSpheres;
    }

    var rows = Enumerable
      .Range(0, input.Count)
      .Select(i => spheres.Where(t => t.y == i).Count() * (input.Count - i))
      .ToList();
    rows.Sum()
      .Dump("14a [112773]: ");
    

    int yy = 12;
  }
}
