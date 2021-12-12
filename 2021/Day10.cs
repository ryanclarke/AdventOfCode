namespace AoC2021;

public class Day10
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/10.txt")
                .ToList();
        var corrupt = new Dictionary<char, (char Opener, int Points)>()
        {
            { ')', ('(', 3) },
            { ']', ('[', 57) },
            { '}', ('{', 1197) },
            { '>', ('<', 25137) }
        };
        var incomplete = new Dictionary<char, (char Closer, int Points)>()
        {
            { '(', (')', 1) },
            { '[', (']', 2) },
            { '{', ('}', 3) },
            { '<', ('>', 4) }
        };

        var results = input.Select(line =>
        {
            var stack = new Stack<char>();

            foreach (var c in line)
            {
                if (c is '(' or '[' or '{' or '<')
                {
                    stack.Push(c);
                }
                else if (c is ')' or ']' or '}' or '>')
                {
                    if (corrupt[c].Opener != stack.Pop())
                    {
                        return (Corrupt: true, Chars: new[] { c });
                    }
                }
            }

            return (Corrupt: false, Chars: stack.ToArray());
        })
        .ToList();

        results
            .Where(r => r.Corrupt)
            .SelectMany(r => r.Chars)
            .Sum(c => corrupt[c].Points)
            .Dump("10a (193275): ");

        var scores = results
            .Where(r => !r.Corrupt)
            .Select(r => r.Chars)
            .Select(cs => cs.Aggregate(0L, (a, c) => a * 5 + incomplete[c].Points))
            .OrderBy(x => x)
            .ToList();
        scores[(scores.Count - 1) / 2]
            .Dump("10b (2429644557): ");
    }
}
