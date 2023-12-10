public class Day08
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/08.txt";
  public static void Run()
  {
    var input = File
            .ReadAllLines(InputFilePath)
            .ToList();
    
    var directions = input[0].ToCharArray();
    var network = input[2..]
      .Select(line => Regex.Replace(line, "[^A-Z]+", " ").Trim().Split())
      .ToDictionary(a => a[0], a => (a[1], a[2]));
    
    var path = input[2..]
      .Select(line => Regex.Replace(line, "[^A-Z]+", " ").Trim().Split())
      .SelectMany(a => new List<(string,string)>{($"L{a[0]}", a[1]), ($"R{a[0]}", a[2])})
      .ToDictionary(t => t.Item1, t => t.Item2);

    var steps = 0;
    var currentNode = "AAA";
    while (currentNode != "ZZZ")
    {
      currentNode = path[$"{directions[steps%directions.Length]}{currentNode}"];
      steps++;
    }
    steps.Dump("08a [22411]: ");

    var ghostNodes = network.Keys.Where(n => n.EndsWith('A'));
    var ghostPath = input[2..]
      .Select(line => Regex.Replace(line, "[^A-Z]+", " ").Trim().Split())
      .SelectMany(a => new List<(string,string)>{($"L{a[0]}", a[1]), ($"R{a[0]}", a[2])})
      .ToDictionary(t => t.Item1, t => t.Item2.EndsWith('Z') ? "ZZZ" : t.Item2);
    var finalSteps = new List<long>();
    foreach (var ghostNode in ghostNodes)
    {
      var ghostSteps = 0;
      var gn = ghostNode;
      while (gn != "ZZZ")
      {
        gn = ghostPath[$"{directions[ghostSteps%directions.Length]}{gn}"];
        ghostSteps++;
      }
      finalSteps.Add(ghostSteps);
    }
    finalSteps.Multiply().Dump("08b [11188774513823]: "); // Replace with LCM
  }
}
