using System.ComponentModel.DataAnnotations;

public class Day06
{
  private static readonly string InputFilePath = $"{Config.InputRoot}/06.txt";
  public static void Run()
  {
    new List<(int Time, int Distance)>{
      (47, 400),
      (98, 1213),
      (66, 1011),
      (98, 1540)
    }
      .Select(t => Enumerable
        .Range(0, t.Time)
        .Count(i => i * (t.Time - i) > t.Distance))
      .Multiply()
      .Dump("06a []: ");

    var (time, distance) = (47986698, 400121310111540L);
    long i = time/2;
    for (; i * (time - i) > distance; i--){}
    ((time/2 - i - 1) * 2 + 1).Dump("06b [>26499772]: ");
  }
}
