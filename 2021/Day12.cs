namespace AoC2021;

public class Day12
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/12.txt")
                .Select(line => line.Split('-'))
                .Select(line => (line[0], line[1]))
                .ToList();
        
        var paths = input
            .Select(t => (t.Item2, t.Item1))
            .Concat(input)
            .GroupBy(t => t.Item1)
            .ToDictionary(g => g.Key, g => g.Select(t => t.Item2).ToList());

        FollowPaths("start", new List<string>{ "start" }).Dump("12a (3485): ");
        FollowPaths("start", new List<string>{ "start" }, false).Dump("12b (3485): ");


        int FollowPaths(string start, IEnumerable<string> visited, bool doubleVisit = true)
        {
            return paths[start]
                .Sum(p => p switch
                {
                    "end" => 1,
                    "start" => 0,
                    _ when p == p.ToLower() && visited.Contains(p) && doubleVisit => 0,
                    _ when p == p.ToLower() && visited.Contains(p) && !doubleVisit => FollowPaths(p, visited.Append(p), true),
                    _ when p == p.ToLower() => FollowPaths(p, visited.Append(p), doubleVisit),
                    _ => FollowPaths(p, visited.ToList(), doubleVisit)
                });
        }
    }
}
