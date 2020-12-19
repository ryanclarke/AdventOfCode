using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using FluentAssertions;

namespace AoC2020
{
    [Command("day18")]
    public class Day18 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\18.txt")
                .ToList();

            "1 + 2 * 3 + 4 * 5 + 6".Compute().Should().Be(71);
            "1 + (2 * 3) + (4 * (5 + 6))".Compute().Should().Be(51);
            "2 * 3 + (4 * 5)".Compute().Should().Be(26);
            "5 + (8 * 3 + 9 + 3 * 4 * 3)".Compute().Should().Be(437);
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".Compute().Should().Be(12240);
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".Compute().Should().Be(13632);
            input.Sum(Day18Utils.Compute).Dump().Should().Be(14006719520523);
            
            "1 + 2 * 3 + 4 * 5 + 6".AdvancedCompute().Should().Be(231);
            "1 + (2 * 3) + (4 * (5 + 6))".AdvancedCompute().Should().Be(51);
            "2 * 3 + (4 * 5)".AdvancedCompute().Should().Be(46);
            "5 + (8 * 3 + 9 + 3 * 4 * 3)".AdvancedCompute().Should().Be(1445);
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".AdvancedCompute().Should().Be(669060);
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".AdvancedCompute().Should().Be(23340);
            input.Sum(Day18Utils.AdvancedCompute).Dump().Should().Be(545115449981968);

            return default;
        }

        public readonly struct Num
        {
            private readonly int _val;

            public Num(int i)
            {
                _val = i;
            }

            public static Num operator +(Num a, Num b) => new Num(a._val + b._val);
            public static Num operator -(Num a, Num b) => new Num(a._val * b._val);
        }
    }

    public static class Day18Utils
    {
        public static long Compute(this string expression)
        {
            var accum = new long[10];
            var level = 0;
            var op = Enumerable.Range(0, 10).Select(_ => Op.Add).ToArray();
            foreach (var c in expression)
            {
                switch (c)
                {
                    case var _ when long.TryParse(c.ToString(), out var i):
                        accum[level] = op[level] switch
                        {
                            Op.Add => accum[level] + i,
                            Op.Mult => (accum[level] == 0 ? 1 : accum[level]) * i,
                            _ => throw new ArgumentOutOfRangeException()
                        };
                        break;
                    case '(':
                        level++;
                        break;
                    case ')':
                        level--;
                        accum[level] = op[level] switch
                        {
                            Op.Add => accum[level] + accum[level + 1],
                            Op.Mult => accum[level] * accum[level + 1],
                            _ => throw new ArgumentOutOfRangeException()
                        };
                        accum[level + 1] = 0;
                        op[level + 1] = Op.Add;
                        break;
                    case '+':
                        op[level] = Op.Add;
                        break;
                    case '*':
                        op[level] = Op.Mult;
                        break;
                    case ' ':
                        break;
                }
            }

            return accum[0];
        }
        
        public static long AdvancedCompute(this string expression)
        {
            var accum = new List<long> {0};
            var parens = new List<int> {0};
            foreach (var c in expression)
            {
                switch (c)
                {
                    case var _ when long.TryParse(c.ToString(), out var i):
                        accum[^1] += i;
                        break;
                    case '+':
                        break;
                    case '*':
                        accum = accum.Append(0).ToList();
                        break;
                    case '(':
                        parens = parens.Append(accum.Count).ToList();
                        accum = accum.Append(0).ToList();
                        break;
                    case ')':
                        accum[parens[^1] - 1] += accum.Skip(parens[^1]).Aggregate(1L, (a, l) => a * l);
                        accum = accum.Take(parens[^1]).ToList();
                        parens = parens.Take(parens.Count - 1).ToList();
                        break;
                }
            }

            return accum.Aggregate(1L, (a, l) => a * l);
        }

        private enum Op
        {
            Add,
            Mult
        }
    }
}