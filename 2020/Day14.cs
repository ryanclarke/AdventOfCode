using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using MoreLinq.Extensions;

namespace AoC2020
{
    [Command("day14")]
    public class Day14 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\14.txt")
                .ToArray();

            var mask = string.Empty;
            var memA = new Dictionary<long, long>();
            var memB = new Dictionary<string, long>();
            foreach (var line in input)
            {
                if (line.StartsWith("mask"))
                {
                    mask = line[7..];
                }
                else
                {
                    var r = Regex.Match(line, @"^mem\[(?<mem>\d+)\] = (?<value>\d+)$");
                    var key = r.Groups["mem"].Value.Map(long.Parse);
                    var value = r.Groups["value"].Value.Map(long.Parse);
                    memA[key] = VersionA(value, mask);
                    var versionB = VersionB(key, mask);
                    foreach (var k in versionB)
                    {
                        memB[k] = value;
                    }
                }
            }

            memA.Values.Sum().Dump();
            memB.Values.Sum().Dump();

            return default;
        }

        private IEnumerable<string> VersionB(long key, string mask)
        {
            var bits = key
                .Map(l => Convert.ToString(l, 2).PadLeft(36, '0'))
                .Zip(mask);
            
            var numberOfXs = mask.Count(c => c == 'X');
            var keys = new List<string>();
            var permutations = Math.Pow(2, numberOfXs);
            for (var i = 0; i < permutations; i++)
            {
                var iter = Convert.ToString(i, 2).PadLeft(numberOfXs, '0');
                var idx = 0;
                var resultingKeyChars = new List<char>();
                foreach (var (keyBit, maskBit) in bits)
                {
                    resultingKeyChars.Add(maskBit switch
                    {
                        '0' => keyBit,
                        '1' => '1',
                        'X' => iter[idx++],
                        _ => throw new Exception("Bad char")
                    });
                }

                var resultingKey = string.Concat(resultingKeyChars);
                keys.Add(resultingKey);
            }

            return keys;
        }

        private static long VersionA(long value, string mask)
        {
            return value
                .Map(l => Convert.ToString(l, 2).PadLeft(36, '0'))
                .Zip(mask)
                .Select(t => t.Second == 'X' ? t.First : t.Second)
                .Map(string.Concat)
                .Map(s => Convert.ToInt64(s, 2));
        }
    }
}

//1270578799545 too low