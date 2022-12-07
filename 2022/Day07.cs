namespace AoC2022;

public class Day07
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/07.txt";
    public static void Run()
    {
        var input = File
            .ReadAllLines(InputFilePath)
            .ToList();

        var root = new Directory("/");
        var pwd = root;
        var allDirs = new List<Directory>{root};
        foreach (var line in input)
        {
            var _ = line.Split() switch
            {
                ["$", "cd", var name] => name switch
                {
                    "/" => pwd = root,
                    ".." => pwd = pwd.Parent,
                    _ => pwd = pwd.Directories.Single(d => d.Name == name)
                },
                ["$", ..] => pwd,
                ["dir", var name] => new Directory(name, pwd)
                    .T(dir => pwd.Directories.Add(dir))
                    .T(dir => allDirs.Add(dir))
                    .X(d => pwd),
                [var size, var name] => pwd.T(p => p.Files.Add(int.Parse(size))),
                _ => pwd,
            };
        }

        var sizes = allDirs.Select(d => d.Size()).OrderDescending().ToList();
        sizes.Where(i => i <= 100000).Sum().Dump("07a (919137): ");

        var needed = 30_000_000 - (70_000_000 - root.Size());
        sizes.Where(s => s >= needed).Min().Dump("07b (2877389): ");
    }

    public class Directory
    {
        public string Name { get; }
        public Directory Parent { get; }
        public List<int> Files { get; init; } = new();
        public List<Directory> Directories { get; init; } = new();

        public Directory(string name, Directory? parent = null)
        {
            Name = name;
            Parent = parent ?? this;
        }

        public int Size() => Files.Sum() + Directories.Sum(d => d.Size());
    }
}
