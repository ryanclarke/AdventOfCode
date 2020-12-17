using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using FluentAssertions;

namespace AoC2020
{
    [Command("day15")]
    public class Day15 : AocCommand
    {
        protected override ValueTask Run()
        {
            new[] {0,3,6}.Spoken(2020).Should().Be(436);
            new[] {1,3,2}.Spoken(2020).Should().Be(1);
            new[] {2,1,3}.Spoken(2020).Should().Be(10);
            new[] {1,2,3}.Spoken(2020).Should().Be(27);
            new[] {2,3,1}.Spoken(2020).Should().Be(78);
            new[] {3,2,1}.Spoken(2020).Should().Be(438);
            new[] {3,1,2}.Spoken(2020).Should().Be(1836);
            new[] {11,18,0,20,1,7,16}.Spoken(2020).Should().Be(639);
            
            
            new[] {0,3,6}.Spoken(30000000).Should().Be(175594);
            new[] {1,3,2}.Spoken(30000000).Should().Be(2578);
            new[] {2,1,3}.Spoken(30000000).Should().Be(3544142);
            new[] {1,2,3}.Spoken(30000000).Should().Be(261214);
            new[] {2,3,1}.Spoken(30000000).Should().Be(6895259);
            new[] {3,2,1}.Spoken(30000000).Should().Be(18);
            new[] {3,1,2}.Spoken(30000000).Should().Be(362);
            new[] {11,18,0,20,1,7,16}.Spoken(30000000).Should().Be(266);

            return default;
        }
    }
    

    public static class Day15Extra
    {
        public static int Spoken(this int[] input, int number)
        {
            var memory = input
                .Select((n, idx) => (idx: idx, number: n))
                .ToDictionary(
                    x => x.number,
                    x => x.idx);

            var lastSpoken = 0;
            for (var i = input.Length; i < number-1; i++)
            {
                var newSpoken = memory.ContainsKey(lastSpoken) ? i - memory[lastSpoken] : 0;
                memory[lastSpoken] = i;
                lastSpoken = newSpoken;
            }

            $"[{string.Join(',',input)}] -> {number} = {lastSpoken}".Dump();
            return lastSpoken;
        }
    }
}