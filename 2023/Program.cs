global using System.Diagnostics;
global using System.Text.RegularExpressions;


var days = System.Reflection.Assembly
    .GetExecutingAssembly()
    .GetTypes()
    .Where(t => t.Name.StartsWith("Day") && t.Name != "Day00");

if (!args.Any())
{
    RunDay(days.MaxBy(t => t.Name));
}
else
{
    Environment.CurrentDirectory = "C:/dev/AdventofCode/2023/bin/Debug/net8.0/";

    if (int.TryParse(args[0], out int dayNumber))
    {
        RunDay(days.First(t => t.Name == $"Day{dayNumber:00}"));
    }
    foreach (var day in days)
    {
        RunDay(day);
    }
}

static void RunDay(Type? day)
{
    var stopwatch = Stopwatch.StartNew();
    
    day?.GetMethod("Run")?.Invoke(day, null);
    
    stopwatch.Stop();
    Console.WriteLine();
    stopwatch.Elapsed.Dump($"{day?.Name} Elapsed: ");
}

public static class Config {
    public static readonly string InputRoot = "C:/dev/AdventOfCode/2023/input/";
}