using System.Collections.Immutable;
using System.Data;

using Cache = System.Collections.Generic.Dictionary<Log, long>;

public class Day12
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/12.txt";

  private static Cache cache = new();
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();

    Solve(input, 1).Dump("12a [7251]: ");
    Solve(input, 5).Dump("12b []: ");
  }

  public static long Solve(List<string> input, int times)
  {
    return input
      .Select(line => line
        .Arrayify()
        .X(a => {
            var pattern = Enumerable.Repeat(a[0], times).JoinStrings("?");
            var numbers = Enumerable.Repeat(a[1], times)
              .JoinStrings(",")
              .Split(',')
              .Select(int.Parse)
              .ToArray()
              .Reverse();
            return Compute(new Log(pattern, ImmutableStack.CreateRange(numbers)));
        }))
      .Sum();
  }

  public static long Compute(Log log)
  {
    if (!cache.TryGetValue(log, out long value))
    {
      value = log.Pattern.FirstOrDefault() switch
      {
        '.' => Compute(log.WithRest()),
        '?' => Compute(log.WithRest(".")) + Compute(log.WithRest("#")),
        '#' => ProcessHash(log),
        _ => log.Numbers.IsEmpty ? 1 : 0,
      };
      cache[log] = value;
    }
    return value;
  }

  public static long ProcessHash(Log log)
  {
    if (log.Numbers.IsEmpty) return 0;

    var numbers = log.Numbers.Pop(out int group);
    
    var maybeBroken = log.Pattern.TakeWhile(c => c == '#' || c == '?').Count();
    
    if (maybeBroken < group) return 0;
    if (log.Pattern.Length == group) return Compute(new Log("", numbers));
    if (log.Pattern[group] == '#') return 0;
    return Compute(new Log(log.Pattern[(group+1)..], numbers));
  }
}

public record Log(string Pattern, ImmutableStack<int> Numbers)
{
  public Log WithRest(string prepend = "") => this with { Pattern = prepend + this.Pattern[1..] };
};
