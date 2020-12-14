using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day08")]
    public class Day08 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\08.txt")
                .ToList();

            var instructions = input
                .Select(s => Regex.Match(s.Trim(), @"^(?<operation>acc|jmp|nop) (?<argument>[-+]\d+)"))
                .Select(m => new Instruction(m.Groups["operation"].Value, int.Parse(m.Groups["argument"].Value)))
                .ToList();
            
            RunCode(instructions).Accumulator.Dump();

            for (var i = 0; i < instructions.Count; i++)
            {
                var (accumulator, finished) = RunCode(instructions, i);
                if (finished)
                {
                    accumulator.Dump();
                    break;
                }
            }

            return default;
        }

        private static (int Accumulator, bool Finished) RunCode(List<Instruction> instructions, int swapInstruction = -1)
        {
            var visited = new HashSet<int>();
            var pointer = 0;
            var accumulator = 0;

            while (!visited.Contains(pointer) && pointer < instructions.Count)
            {
                visited.Add(pointer);
                var op = pointer == swapInstruction
                    ? instructions[pointer].Op == "nop" ? "jmp" :
                    instructions[pointer].Op == "jmp" ? "nop" : "acc"
                    : instructions[pointer].Op;
                switch (op)
                {
                    case "nop":
                        pointer += 1;
                        break;
                    case "acc":
                        accumulator += instructions[pointer].Arg;
                        pointer += 1;
                        break;
                    case "jmp":
                        pointer += instructions[pointer].Arg;
                        break;
                    default:
                        throw new Exception("Invalid operation");
                }
            }
            return (accumulator, pointer >= instructions.Count);
        }

        private record Instruction(string Op, int Arg);
    }
}