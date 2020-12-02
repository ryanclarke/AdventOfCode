using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CliFx;
using CliFx.Attributes;

namespace AoC2020
{
    [Command(name: "day01")]
    public class Day01 : ICommand
    {
        public ValueTask ExecuteAsync(IConsole console)
        {
            var expenses = File
                .ReadAllLines("input/01.txt")
                .Select(int.Parse)
                .ToList();

            var output = expenses
                .SelectMany((a, idxA) => expenses
                    .Skip(idxA)
                    .Select((b, idxB) => (a + b, a * b)))
                .First(x => x.Item1 == 2020)
                .Item2;
            console.Output.WriteLine(output);
            
            output = expenses
                .SelectMany((a, idxA) => expenses
                    .Skip(idxA)
                    .SelectMany((b, idxB) => expenses
                        .Skip(idxB)
                        .Select((c, idxC) => (a + b + c, a * b * c))))
                .First(x => x.Item1 == 2020)
                .Item2;
            console.Output.WriteLine(output);

            return default;
        }
    }
}