using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using FluentAssertions;

namespace AoC2020
{
    [Command("day23")]
    public class Day23 : AocCommand
    {
        protected override ValueTask Run()
        {
            Play(389125467, 9, 10).Dump().Should().Be(92658374);
            Play(389125467, 9, 100).Dump().Should().Be(67384529);
            Play(193467258, 9, 100).Dump().Should().Be(25468379);
            
            Play(389125467, 1_000_000, 10_000_000).Dump().Should().Be(149245887792L);
            Play(193467258, 1_000_000, 10_000_000).Dump().Should().Be(474747880250L);
            
            return default;
        }

        private long Play(int cups, int max, int moves)
        {
            var numbers = cups.ToString()
                .Select(c => int.Parse(c.ToString()))
                .ToArray();
            if (max > 9)
            {
                numbers = numbers
                    .Concat(Enumerable.Range(10, max - 9))
                    .ToArray();
            }

            var previous = numbers.Last();
            var circle = new int[max + 1];
            foreach (var i in numbers)
            {
                circle[previous] = i;
                previous = i;
            }

            var current = numbers.Last();
            for (int i = 0; i < moves; i++)
            {
                var active = circle[current];
                var theThree1 = circle[active];
                var theThree2 = circle[theThree1];
                var theThree3 = circle[theThree2];
                var theRest = circle[theThree3];

                var target = active - 1;
                while ( target == theThree1 || target == theThree2 || target == theThree3 || target <= 0)
                {
                    target -= 1;

                    if (target <= 0)
                    {
                        target = max;
                    }
                }

                current = circle[current];
                var afterTarget = circle[target];
                circle[target] = theThree1;
                circle[theThree3] = afterTarget;
                circle[active] = theRest;
            }

            if (max == 9)
            {
                return Stringify(circle, 1)[..^1].Map(long.Parse);
            }
            var firstStar = circle[1];
            var secondStar = circle[firstStar];
            return firstStar * (long)secondStar;
        }

        private string Stringify(int[] circle, int current)
        {
            var pointer = current;
            var s = "";
            while (circle[pointer] != current)
            {
                s += circle[pointer].ToString();
                pointer = circle[pointer];
            }

            s += circle[pointer];

            return s;
        }
    }
}