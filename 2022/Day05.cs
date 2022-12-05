using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;

namespace AoC2022;

public class Day05
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/05.txt";
    public static void Run()
    {
        var input = File.ReadAllText(InputFilePath);

        var (stacksA, stacksB, procedures) = input
                .Split("\n\n")
                .X(a =>
                    (GenerateStacks(a[0]),
                    GenerateStacks(a[0]),
                    a[1]
                        .Split("\n", StringSplitOptions.RemoveEmptyEntries)
                        .Select(line => line
                                .Split(" ")
                                .X(a => Step.New(a[1], a[3], a[5])))
                        .ToList())
                    );

        foreach (var step in procedures)
        {
            for (int i = 0; i < step.Move; i++)
            {
                stacksA[step.To].Push(stacksA[step.From].Pop());
            }
        }
        stacksA.Select(s => s.Peek()).Stringify().Dump("05a (FWSHSPJWM) :");

        foreach (var step in procedures)
        {
            for (int i = 0; i < step.Move; i++)
            {
                stacksB[0].Push(stacksB[step.From].Pop());
            }
            for (int i = 0; i < step.Move; i++)
            {
                stacksB[step.To].Push(stacksB[0].Pop());
            }
        }
        stacksB.Select(s => s.Peek()).Stringify().Dump("05b (PWPWHGFZS) :");
    }

    private static List<Stack<char>> GenerateStacks(string input) =>
        Enumerable.Range(0, 9)
            .Select(i => input
                    .Split("\n")
                    .Reverse()
                    .Skip(1)
                    .Select(line => line.PadRight(35))
                    .Select(row => row[1 + i * 4])
                    .Where(char.IsAsciiLetterUpper)
                    .X(x => new Stack<char>(x)))
            .Prepend(new Stack<char>(" "))
            .ToList();

    private record Step(int Move, int From, int To)
    {
        public static Step New(string Move, string From, string To) =>
            new(int.Parse(Move), int.Parse(From), int.Parse(To));
    };
}
