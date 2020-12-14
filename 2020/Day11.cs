using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day11")]
    public class Day11 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\11.txt")
                .ToArray();

            PartA(input);
            PartB(input);

            return default;
        }

        private static void PartA(string[] input)
        {
            var map = string.Join(string.Empty, input);
            var width = input[0].Length;
            var height = input.Length;
            var sites = map.Length;

            var neighbors = input
                .SelectMany((row, y) => row
                    .SelectMany((c, x) => new[]
                        {
                            (Y: y - 1, X: x - 1),
                            (Y: y - 1, X: x),
                            (Y: y - 1, X: x + 1),
                            (Y: y, X: x - 1),
                            (Y: y, X: x + 1),
                            (Y: y + 1, X: x - 1),
                            (Y: y + 1, X: x),
                            (Y: y + 1, X: x + 1)
                        }
                        .Where(p =>
                            p.Y >= 0
                            && p.Y < height
                            && p.X >= 0
                            && p.X < width)
                        .Select(p => (index: y * width + x, neighbor: p.Y * width + p.X))
                        .ToArray())
                    .ToArray())
                .GroupBy(t => t.index)
                .ToDictionary(
                    g => g.Key,
                    g => g.Select(t => t.neighbor).ToArray());

            var start = string.Empty;
            var end = map;
            while (start != end)
            {
                start = end;
                end = string.Concat(start
                    .Select((c, i) => c switch
                    {
                        '.' => '.',
                        'L' => neighbors[i].Any(x => start[x] == '#') ? 'L' : '#',
                        '#' => neighbors[i].Count(x => start[x] == '#') >= 4 ? 'L' : '#',
                        _ => throw new Exception("Invalid cell type")
                    }));

                // var state = string.Join(Environment.NewLine, end.Batch(width).Select(x => string.Concat(x)));
                // state.Dump();
                // string.Empty.Dump();
            }

            end.Count(c => c == '#').Dump();
        }

        private static void PartB(string[] input)
        {
            var map = string.Join(string.Empty, input);
            var width = input[0].Length;
            var height = input.Length;
            var sites = map.Length;

            (int y, int x)? VisibleChair(int y, int x, int dy, int dx) =>
                (y + dy, x + dx) switch
                {
                    (int _y, int _x) when
                        _y >= 0
                        && _y < height
                        && _x >= 0
                        && _x < width => input[_y][_x] switch
                        {
                            '.' => VisibleChair(_y, _x, dy, dx),
                            _ => (_y, _x)
                        },
                    _ => null
                };

            var neighbors = input
                .SelectMany((row, y) => row
                    .SelectMany((c, x) => new[]
                        {
                            VisibleChair(y, x, -1, -1),
                            VisibleChair(y, x, -1, 0),
                            VisibleChair(y, x, -1, 1),
                            VisibleChair(y, x, 0, -1),
                            VisibleChair(y, x, 0, 1),
                            VisibleChair(y, x, 1, -1),
                            VisibleChair(y, x, 1, 0),
                            VisibleChair(y, x, 1, 1)
                        }
                        .Where(p => p != null)
                        .Cast<(int y, int x)>()
                        .Select(p => (index: y * width + x, neighbor: p.y * width + p.x))
                        .ToArray())
                    .ToArray())
                .GroupBy(t => t.index)
                .ToDictionary(
                    g => g.Key,
                    g => g.Select(t => t.neighbor).ToArray());

            var start = string.Empty;
            var end = map;
            while (start != end)
            {
                start = end;
                end = string.Concat(start
                    .Select((c, i) => c switch
                    {
                        '.' => '.',
                        'L' => neighbors[i].Any(x => start[x] == '#') ? 'L' : '#',
                        '#' => neighbors[i].Count(x => start[x] == '#') >= 5 ? 'L' : '#',
                        _ => throw new Exception("Invalid cell type")
                    }));

                // var state = string.Join(Environment.NewLine, end.Batch(width).Select(x => string.Concat(x)));
                // state.Dump();
                // string.Empty.Dump();
            }

            end.Count(c => c == '#').Dump();
        }
    }
}