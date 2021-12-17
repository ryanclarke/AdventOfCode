﻿global using System.Diagnostics;
global using AoC2021;

var days = System.Reflection.Assembly
    .GetExecutingAssembly()
    .GetTypes()
    .Where(t => t.Name.StartsWith("Day") && t.Name != "Day00");

if (args.Count() == 0)
{
    RunDay(days.MaxBy(t => t.Name));
}
else
{
    Environment.CurrentDirectory = "C:/dev/AdventofCode/2021/bin/Debug/net6.0/";

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