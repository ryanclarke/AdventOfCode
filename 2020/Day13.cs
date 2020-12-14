using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using MoreLinq.Extensions;

namespace AoC2020
{
    [Command("day13")]
    public class Day13 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\13.txt")
                .ToArray();

            // input = new[]
            // {
            //     "939",
            //     "7,13,x,x,59,x,31,19"
            // };

            var timestamp = int.Parse(input[0]);
            input[1]
                .Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                .Where(busId => busId != "x")
                .Select(int.Parse)
                .Select(busId => (id: busId, wait: busId - (timestamp % busId)))
                .OrderBy(bus => bus.wait)
                .First()
                .Dump()
                .Tee(bus => (bus.id * bus.wait).Dump());

            new[]
            {
                "17,x,13,19",
                "67,7,59,61",
                "67,x,7,59,61",
                "67,7,x,59,61",
                "1789,37,47,1889"
            }.ForEach(PartB);
            PartB(input[1]);

            return default;
        }

        private static void PartB(string input)
        {
            var buses = input
                .Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                .Select((s, i) => (idx: i, bus: s == "x" ? 1 : long.Parse(s)))
                .Where(x => x.bus > 1)
                .OrderByDescending(x => x.bus)
                .ToList();

            var start = -(long) buses[0].idx;
            for (int i = 2; i <= buses.Count; i++)
            {
                start = Start(buses, i, start);
            }

            start.Dump();
        }

        private static long Start(List<(int idx, long bus)> buses, int number, long start)
        {
            var myBuses = buses.Take(number);
            var repeat = myBuses.Take(number-1).Aggregate(1L, (l, b) => l * b.bus);
            while (!myBuses.All(bus => (start + bus.idx) % bus.bus == 0))
            {
                start += repeat;
            }

            return start;
        }
    }
}