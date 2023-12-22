public class Day13
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/13test.txt";
  public static void Run()
  {
    var input = File
            .ReadAllText(InputFilePath)
            .Split("\n\n")
            .Select(note => note.Split('\n'))
            .ToList();

    input
      .Select(note =>
        Enumerable
          .Range(1, note.Length - 1)
          .Select(i =>
          {
            var overlap = Math.Min(i, note.Length - i);
            var top = note[..i].Reverse().Take(overlap).ToList();
            var bottom = note[i..].Take(overlap).ToList();
            var mirrored = top.Zip(bottom).All(x => x.Item1 == x.Item2);
            return mirrored ? i * 100 : 0;
          })
          .Sum()
        +
        Enumerable
          .Range(1, note[0].Length - 1)
          .Select(i =>
          {
            var overlap = Math.Min(i, note[0].Length - i);
            var top = Enumerable.Range(i - overlap, overlap).Select(x => note.Select(l => l[x]).Stringify()).Reverse().ToList();
            var bottom = Enumerable.Range(i, overlap).Select(x => note.Select(l => l[x]).Stringify()).ToList();
            var mirrored = top.Zip(bottom).All(x => x.Item1 == x.Item2);
            return mirrored ? i : 0;
          })
          .Sum())
      .Sum().Dump("13a [33195]: ");

    var score = 0;
    var skip = false;
    foreach (var noteO in input)
    {
      for (var y = 0; y < noteO.Length; y++)
      {
        for (var x = 0; x < noteO[0].Length; x++)
        {
          var note = noteO
            .Select((line, iy) => iy != y ? line : line
              .Select((c, ix) => ix != x ? c : c == '.' ? '#' : '.')
              .Stringify())
            .ToArray();
          var myScore =
              Enumerable
                .Range(1, note.Length - 1)
                .Select(i =>
                {
                  var overlap = Math.Min(i, note.Length - i);
                  var top = note[..i].Reverse().Take(overlap).ToList();
                  var bottom = note[i..].Take(overlap).ToList();
                  var mirrored = top.Zip(bottom).All(x => x.Item1 == x.Item2);
                  return mirrored ? i * 100 : 0;
                })
                .Sum();
              // +
              // Enumerable
              //   .Range(1, note[0].Length - 1)
              //   .Select(i =>
              //   {
              //     var overlap = Math.Min(i, note[0].Length - i);
              //     var top = Enumerable.Range(i - overlap, overlap).Select(x => note.Select(l => l[x]).Stringify()).Reverse().ToList();
              //     var bottom = Enumerable.Range(i, overlap).Select(x => note.Select(l => l[x]).Stringify()).ToList();
              //     var mirrored = top.Zip(bottom).All(x => x.Item1 == x.Item2);
              //     return mirrored ? i : 0;
              //   })
              //   .Sum();
          if (myScore > 0)
          {
            score += myScore;
            skip = true;
            break;
          }
        }
        if (skip)
        {
          skip = false;
          break;
        }
      }
    }


    score.Dump("13b [<3150585]: ");

    int yy = 12;
  }
}
