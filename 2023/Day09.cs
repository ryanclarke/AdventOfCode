public class Day09
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/09.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
    
    var pyramids = input
      .Select(line => line.Arrayify().Select(int.Parse).ToArray())
      .Select(seq => {
        var pyramid = new List<int[]>{seq};
        while (pyramid.Last().Any(i => i != 0))
        {
          var last = pyramid.Last();
          var next = last[1..].Zip(last.Take(last.Length - 1)).Select(t => t.First - t.Second).ToArray();
          pyramid.Add(next);
        }
        return pyramid;
      })
      .ToArray();
    
    pyramids
      .Select(pyramid => pyramid
        .Select(ints => ints.Last())
        .Sum())
      .Sum()
      .Dump("09a [1842168671]: ");
    
    pyramids
      .Select(pyramid => pyramid
        .Select(row => row.First())
        .Reverse()
        .Aggregate(0, (a, i) => i - a))
      .Sum()
      .Dump("09b [903]: ");
  }
}
