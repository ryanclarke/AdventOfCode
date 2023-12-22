public class Day19
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/19.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();

    var inputSplit = input.IndexOf("");
    var workflows = input[..inputSplit]
      .Select(Workflow.Parse)
      .ToDictionary(x => x.Name, x => x.Run);
    
    var parts = input[(inputSplit+1)..]
      .Select(Part.Parse)
      .ToList();

    var count = 0d;
    foreach (var part in parts)
    {
      var next = workflows["in"](part);
      while (next != "A" && next != "R")
      {
        next = workflows[next](part);
      }
      if (next == "A")
      {
        count += part.Score;
      }
    }
    count.Dump("19a [323625]: ");
  }

  public record Workflow(string Name, Func<Part, string> Run)
  {
    public static Workflow Parse(string line)
    {
      var split = Regex.Split(line, "[{}]");
      var rules = Regex.Split(split[1], ",")
        .Select<string, Func<Part, string, string>>(raw => {
          Func<Part, string> newNext = _ => "";
          if (raw.Contains(':'))
          {
            var div = raw.Split(':');
            var val = int.Parse(div[0][2..]);
            var check = div[0][..2];
            newNext = check switch
            {
              "x<" => part => part.X < val ? div[1] : "",
              "x>" => part => part.X > val ? div[1] : "",
              "m<" => part => part.M < val ? div[1] : "",
              "m>" => part => part.M > val ? div[1] : "",
              "a<" => part => part.A < val ? div[1] : "",
              "a>" => part => part.A > val ? div[1] : "",
              "s<" => part => part.S < val ? div[1] : "",
              "s>" => part => part.S > val ? div[1] : "",
              _ => throw new ArgumentException()
            };
          }
          else
          {
            newNext = _ => raw;
          }
          return (Part part, string next) => !string.IsNullOrWhiteSpace(next) ? next : newNext(part);
        })
        .ToList();
      return new Workflow(split[0], part => rules.Aggregate((part, ""), (a, r) => (a.part, r(a.part, a.Item2))).Item2);
    }
  }

  public record Part(int X, int M, int A, int S)
  {
    public long Score = X + M + A + S;
    public static Part Parse(string line)
    {
      var ints = Regex.Matches(line, "[0-9]+")
        .Select(m => int.Parse(m.Value))
        .ToArray();
      return new Part(ints[0], ints[1], ints[2], ints[3]);
    }
  }
}
