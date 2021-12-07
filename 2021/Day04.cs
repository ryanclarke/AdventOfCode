namespace AoC2021;

public class Day04
{
    public static void Run()
    {
        var input = File
                .ReadAllText("../../../input/04.txt")
                .Split("\n\n")
                .ToList();

        var drawOrder = input.First().Split(',').Select(int.Parse).ToList();
        var boards = input.Skip(1)
                    .Select(x => x.Trim()
                        .Replace("\n", " ")
                        .Replace("  ", " ")
                        .Split(' ')
                        .Select(int.Parse)
                        .ToArray())
                    .ToList().Select(x => new Board(x, x
                .Chunk(5)
                .Concat(Enumerable.Range(0, 5).Select(i => (new int[] { x[i], x[i + 5], x[i + 10], x[i + 15], x[i + 20] })))
                .ToList()))
            .ToList();

        for (int i = 5; i < drawOrder.Count; i++)
        {
            var drawn = drawOrder.Take(i).ToList();

            var bingos = boards.FindAll(b => b.Rows.Any(r => r.All(drawn.Contains)));

            foreach (var bingo in bingos)
            {
                boards.Remove(bingo);
                var score = bingo.Spaces.Where(s => !drawn.Contains(s)).Sum() * drawn.Last();
                if (boards.Count == 99)
                {
                    score.Dump("4a: ");
                }
                if (boards.Count == 0)
                {
                    score.Dump("4b: ");
                }
            }
        }
    }

    private record Board(int[] Spaces, List<int[]> Rows);
}
