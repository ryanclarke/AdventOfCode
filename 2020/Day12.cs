using System;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day12")]
    public class Day12 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\12.txt")
                .ToArray();

            // input = new[]
            // {
            //     "F10",
            //     "N3",
            //     "F7",
            //     "R90",
            //     "F11"
            // };

            PartA(input);
            PartB(input);

            return default;
        }

        private void PartA(string[] input)
        {
            var ship = input
                .Select(l => Regex.Match(l, @"^(?<action>[NSEWLRF])(?<value>\d+)"))
                .Select(m => new Instruction(
                    Enum.Parse<Act>(m.Groups["action"].Value),
                    int.Parse(m.Groups["value"].Value)))
                .Aggregate(
                    new Ship(Heading.E, 0, 0),
                    (ship, instruction) => instruction.action switch
                    {
                        Act.L => ship with {heading =
                            (Heading) ((360 + (int) ship.heading + (int) Act.L * instruction.value) % 360) },
                        Act.R => ship with {heading =
                            (Heading) ((360 + (int) ship.heading + (int) Act.R * instruction.value) % 360) },
                        Act.F => UpdateShip(ship.heading, ship, instruction.value),
                        Act a => UpdateShip((Heading) a, ship, instruction.value)
                    }
                );
            ship.Dump();
            (Math.Abs(ship.x) + Math.Abs(ship.y)).Dump();
        }

        private void PartB(string[] input)
        {
            var shipStatus = input
                .Select(l => Regex.Match(l, @"^(?<action>[NSEWLRF])(?<value>\d+)"))
                .Select(m => new Instruction(
                    Enum.Parse<Act>(m.Groups["action"].Value),
                    int.Parse(m.Groups["value"].Value)))
                .Aggregate(
                    new ShipStatus(1, 10, 0, 0),
                    (s, i) => i.action switch
                        {
                            Act.N => new ShipStatus(s.hy + i.value, s.hx, s.sy, s.sx),
                            Act.S => new ShipStatus(s.hy - i.value, s.hx, s.sy, s.sx),
                            Act.E => new ShipStatus(s.hy, s.hx + i.value, s.sy, s.sx),
                            Act.W => new ShipStatus(s.hy, s.hx - i.value, s.sy, s.sx),
                            Act.F => new ShipStatus(s.hy, s.hx, s.sy + (s.hy * i.value), s.sx + (s.hx * i.value)),
                            Act.L => (i.value switch
                            {
                                90 => new ShipStatus(s.hx, -s.hy, s.sy, s.sx),
                                180 => new ShipStatus(-s.hy, -s.hx, s.sy, s.sx),
                                270 => new ShipStatus(-s.hx, s.hy, s.sy, s.sx),
                                _ => throw new Exception("Wrong angle")
                            }),
                            Act.R => (i.value switch
                            {
                                90 => new ShipStatus(-s.hx, s.hy, s.sy, s.sx),
                                180 => new ShipStatus(-s.hy, -s.hx, s.sy, s.sx),
                                270 => new ShipStatus(s.hx, -s.hy, s.sy, s.sx),
                                _ => throw new Exception("Wrong angle")
                            })
                        });
            shipStatus.Dump();
            (Math.Abs(shipStatus.sx) + Math.Abs(shipStatus.sy)).Dump();
        }

        private Ship UpdateShip(Heading heading, Ship ship, int value) => heading switch
        {
            Heading.E => ship with {x = ship.x + value},
            Heading.W => ship with {x = ship.x - value},
            Heading.N => ship with {y = ship.y + value},
            Heading.S => ship with {y = ship.y - value}
        };

        private enum Act
        {
            R = -1,
            E = 0,
            L = 1,
            F = 8,
            N = 90,
            W = 180,
            S = 270
        }
        private enum Heading
        {
            E = 0,
            N = 90,
            W = 180,
            S = 270
        }
        private record Instruction(Act action, int value);
        private record Ship(Heading heading, int x, int y);
        private record ShipStatus(int hy, int hx, int sy, int sx);
    }
}