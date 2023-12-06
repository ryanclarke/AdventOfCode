using System.Data;

public class Day05
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/05.txt";
  public static void Run()
  {
    var input = File
            .ReadAllText(InputFilePath)
            .Split("\n\n", StringSplitOptions.RemoveEmptyEntries & StringSplitOptions.TrimEntries)
            .ToList();

    var seeds = input[0]
      .Trim().Split(':')[1]
      .Arrayify().Select(long.Parse)
      .ToList();
    var rangeOffsets = input[1..]
      .Select(map => map
        .Trim().Split(" map:\n", StringSplitOptions.RemoveEmptyEntries & StringSplitOptions.TrimEntries)[1]
        .Trim().Split('\n', StringSplitOptions.RemoveEmptyEntries & StringSplitOptions.TrimEntries)
        .Select(line => line.Arrayify().Select(long.Parse).ToArray().X(x => new RangeOffset(x[1], x[1] + x[2] - 1, x[0] - x[1])))
        .ToList())
      .ToList();

    seeds
      .Select(seed => rangeOffsets
        .Aggregate(seed, (val, range) =>
          range
            .Where(ro => ro.Contains(val))
            .X(ros => ros.Any() ? val + ros.Single().Offset : val)))
      .Min()
      .Dump("05a [175622908]: ");

    rangeOffsets = rangeOffsets
      .Prepend(seeds
        .Chunk(2)
        .Select(pair => new RangeOffset(pair[0], pair[0] + pair[1] - 1, 0))
        .ToList())
      .ToList();

    var opts = rangeOffsets[0];
    foreach (var stage in rangeOffsets[1..])
    {
      var queue = new Queue<RangeOffset>(opts);
      var finished = new List<RangeOffset>();
      foreach (var offset in stage)
      {
        var countdown = queue.Count;
        while (countdown > 0)
        {
          var (requeue, offseted) = UpdateRanges(queue.Dequeue(), offset);
          foreach (var ro in requeue)
          {
            queue.Enqueue(ro);
          }
          finished.AddRange(offseted);
          if (requeue.Count > 1)
          {
            countdown = queue.Count;
          }
          countdown--;
        }
      }
      opts = [.. queue];
      opts.AddRange(finished);
      opts = opts
        .Select(x => x.Apply())
        .ToList();
    }
    opts.Select(x => x.Start).Min().Dump("05b [5200543]: ");
  }

  public static (List<RangeOffset> Requeue, List<RangeOffset> Offseted) UpdateRanges(RangeOffset a, RangeOffset b)
  {
    if (a.Start >= b.Start && a.End <= b.End) // b contains a
      return ([], [a with { Offset = b.Offset }]);
    if (a.Start <= b.Start && a.End >= b.End) // a contains b
      return ([
        a with { End = b.Start - 1 },
        a with { Start = b.End + 1 }
      ], [
        b
      ]);
    if (a.Start <= b.Start && a.End >= b.Start) // b start overlaps a end
      return ([
        a with { End = b.Start - 1 }
      ], [
        b with { End = a.End }
      ]);
    if (a.Start <= b.End && a.End >= b.End) // b end overlaps a start
      return ([
        a with { Start = b.End + 1 }
      ], [
        b with { Start = a.Start }
      ]);
    return ([a], []); // no overlap
  }

  public record RangeOffset(long Start, long End, long Offset)
  {
    public bool Contains(long val) => val >= Start && val <= End;
    public RangeOffset Apply(long? offset = null)
    {
      return new RangeOffset(Start + (offset ?? Offset), End + (offset ?? Offset), 0);
    }
  };
}
