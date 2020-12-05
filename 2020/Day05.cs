using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CliFx;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day05")]
    public class Day05 : ICommand
    {
        public ValueTask ExecuteAsync(IConsole console)
        {
            var input = File
                .ReadAllLines("input/05.txt")
                .ToList();

            var seatIds = input
                .Select(x => x
                    .Replace('F', '0')
                    .Replace('B', '1')
                    .Replace('L', '0')
                    .Replace('R', '1'))
                .Select(x => Convert.ToInt32(x, 2))
                .OrderBy(x => x);
            
            console.Output.WriteLine(seatIds.Max());

            var mySeatId = seatIds
                .Aggregate(
                    new Accumulator(seatIds.Min(), 0),
                    (acc, id) =>
                        id - acc.lastId == 2 
                            ? new Accumulator(id, id - 1) 
                            : acc with {lastId = id}
                )
                .myId;

            console.Output.WriteLine(mySeatId);

            return default;
        }

        public record Accumulator(int lastId, int myId);
    }
}