public class Day15
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/15.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList()[0]
            .Split(',')
            .ToList();
    
    input
      .Select(step => step
        .Aggregate(0, (a, c) => (a + c) * 17 % 256))
      .Sum()
      .Dump("15a [512283]: ");

    var steps = input
      .Select(s => {
        var label = Regex.Replace(s, "[-=0-9]", "");
        var hash = label.Aggregate(0, (a, c) => (a + c) * 17 % 256);
        return new Step(label, hash, s.Contains('=') ? int.Parse(s.Split('=')[1]) : -1);
      })
      .ToList();

    var boxes = new List<Step>[256].Select(b => new List<Step>()).ToArray();

    foreach (var step in steps)
    {
      var index = boxes[step.Hash].FindIndex(s => s.Label == step.Label);
      if (step.Focal < 0 && index != -1)
      {
        boxes[step.Hash].RemoveAt(index);
      }
      else if (step.Focal >= 0)
      {
        if (index != -1)
        {
          boxes[step.Hash].RemoveAt(index);
          boxes[step.Hash].Insert(index, step);
        }
        else
        {
          boxes[step.Hash].Add(step);
        }
      }
    }

    boxes
      .SelectMany((b, bi) => b
        .Select((s, si) => (bi + 1) * (si + 1) * s.Focal))
      .Sum()
      .Dump("15b [215827]: ");
  }
}

public record Step(string Label, int Hash, int Focal);
