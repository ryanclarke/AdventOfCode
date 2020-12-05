using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CliFx;
using CliFx.Attributes;

namespace AoC2020
{
    [Command("day02")]
    public class Day02 : ICommand
    {
        public ValueTask ExecuteAsync(IConsole console)
        {
            var input = File
                .ReadAllLines("input/02.txt")
                .Select(Parse)
                .ToList();

            console.Output.WriteLine(input.Where(SledShopRule).Count());
            console.Output.WriteLine(input.Where(TobogganShopRule).Count());
            
            return default;
        }

        private bool SledShopRule(Password p)
        {
            var n = p.Value.Count(c => p.Character == c);
            return n >= p.A && n <= p.B;
        }

        private bool TobogganShopRule(Password p)
        {
            var charAtA = p.Value[p.A-1] == p.Character;
            var charAtB = p.Value[p.B-1] == p.Character;
            return charAtA ^ charAtB;
        }

        private Password Parse(string line, int idx)
        {
            var parts = line.Split(new[] {'-', ' ', ':'}, StringSplitOptions.RemoveEmptyEntries);
            return new Password
            {
                A = int.Parse(parts[0]),
                B = int.Parse(parts[1]),
                Character = char.Parse(parts[2]),
                Value = parts[3]
            };
        }
    }

    public record Password
    {
        public int A;
        public int B;
        public char Character;
        public string Value;
    }
}