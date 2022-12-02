global using System.Diagnostics;
global using AoC2022;
global using static AoC2022.Utils;

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
    Environment.CurrentDirectory = "C:/dev/AdventofCode/2022/bin/Debug/net6.0/";

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
    day?.GetMethod("Run")?.Invoke(day, null);
}

public static class Config {
    public static readonly string InputRoot = "C:/dev/AdventOfCode/2022/input/";
}