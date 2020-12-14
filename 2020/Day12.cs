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
            (Math.Abs(ship.x) + Math.Abs(ship.y)).Dump();
        }

        private void PartB(string[] input)
        {
            var shipStatus = new ShipStatus(1, 10, 0, 0);
            foreach (var l in input)
            {
                var m = Regex.Match(l, @"^(?<action>[NSEWLRF])(?<value>\d+)");
                var instruction = new Instruction(Enum.Parse<Act>(m.Groups["action"].Value), int.Parse(m.Groups["value"].Value));
                shipStatus = instruction.action switch
                {
                    Act.N => new ShipStatus(shipStatus.hy + instruction.value, shipStatus.hx, shipStatus.sy, shipStatus.sx),
                    Act.S => new ShipStatus(shipStatus.hy - instruction.value, shipStatus.hx, shipStatus.sy, shipStatus.sx),
                    Act.E => new ShipStatus(shipStatus.hy, shipStatus.hx + instruction.value, shipStatus.sy, shipStatus.sx),
                    Act.W => new ShipStatus(shipStatus.hy, shipStatus.hx - instruction.value, shipStatus.sy, shipStatus.sx),
                    Act.F => new ShipStatus(shipStatus.hy, shipStatus.hx, shipStatus.sy + (shipStatus.hy * instruction.value), shipStatus.sx + (shipStatus.hx * instruction.value)),
                    Act.L => (instruction.value switch
                    {
                        90 => new ShipStatus(shipStatus.hx, -shipStatus.hy, shipStatus.sy, shipStatus.sx),
                        180 => new ShipStatus(-shipStatus.hy, -shipStatus.hx, shipStatus.sy, shipStatus.sx),
                        270 => new ShipStatus(-shipStatus.hx, shipStatus.hy, shipStatus.sy, shipStatus.sx),
                        _ => throw new ArgumentOutOfRangeException("Wrong angle")
                    }),
                    Act.R => (instruction.value switch
                    {
                        90 => new ShipStatus(-shipStatus.hx, shipStatus.hy, shipStatus.sy, shipStatus.sx),
                        180 => new ShipStatus(-shipStatus.hy, -shipStatus.hx, shipStatus.sy, shipStatus.sx),
                        270 => new ShipStatus(shipStatus.hx, -shipStatus.hy, shipStatus.sy, shipStatus.sx),
                        _ => throw new ArgumentOutOfRangeException("Wrong angle")
                    }),
                    _ => throw new ArgumentOutOfRangeException("Wrong action")
                };
            }

            (Math.Abs(shipStatus.sx) + Math.Abs(shipStatus.sy)).Dump();
        }

        private static Ship UpdateShip(Heading heading, Ship ship, int value) => heading switch
        {
            Heading.E => ship with {x = ship.x + value},
            Heading.W => ship with {x = ship.x - value},
            Heading.N => ship with {y = ship.y + value},
            Heading.S => ship with {y = ship.y - value},
            _ => throw new ArgumentOutOfRangeException(nameof(heading), heading, null)
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