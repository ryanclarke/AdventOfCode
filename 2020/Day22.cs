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
    [Command("day22")]
    public class Day22 : AocCommand
    {
        protected override ValueTask Run()
        {
             var input = File
                 .ReadAllText(@"C:\dev\AdventOfCode\2020\input\22.txt")
                 .Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
                 .Select(image => image
                     .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
                     .Skip(1)
                     .Select(int.Parse)
                     .ToList())
                 .ToList();

             var player1 = input[0];
             var player2 = input[1];

             var (_, firstWinningHand) = PlayCombat(player1, player2);
             firstWinningHand
                 .Select((card, idx) => card * (firstWinningHand.Count - idx))
                 .Sum()
                 .Dump().Should().BeOneOf(306, 31269);
             
             var (_, secondWinningHand) = PlayRecursiveCombat(player1, player2);
             secondWinningHand
                 .Select((card, idx) => card * (secondWinningHand.Count - idx))
                 .Sum()
                 .Dump().Should().BeOneOf(291, 31151);

             return default;
        }

        private static (int playerNumber, List<int> winningHand) PlayCombat(List<int> player1, List<int> player2)
        {
            while (player1.Count > 0 && player2.Count > 0)
            {
                var card1 = player1.First();
                var card2 = player2.First();

                player1 = player1.Skip(1).ToList();
                player2 = player2.Skip(1).ToList();

                if (card1 > card2)
                {
                    player1 = player1.Append(card1).Append(card2).ToList();
                }
                else
                {
                    player2 = player2.Append(card2).Append(card1).ToList();
                }
            }

            return player1.Count > player2.Count ? (1, player1) : (2, player2);
        }

        private static (int playerNumber, List<int> winningHand) PlayRecursiveCombat(List<int> player1, List<int> player2)
        {
            static string EncodeHand(List<int> p1, List<int> p2) => $"{string.Join(',', p1)}|{string.Join(',', p2)}";

            var hands = new List<string> { EncodeHand(player1, player2) };
            
            while (player1.Count > 0 && player2.Count > 0)
            {
                var card1 = player1.First();
                var card2 = player2.First();

                player1 = player1.Skip(1).ToList();
                player2 = player2.Skip(1).ToList();

                var shouldRecurse = card1 <= player1.Count && card2 <= player2.Count;
                var winner = shouldRecurse
                    ? PlayRecursiveCombat(player1.Take(card1).ToList(), player2.Take(card2).ToList()).playerNumber
                    : card1 > card2 ? 1 : 2;
                
                if (winner == 1)
                {
                    player1 = player1.Append(card1).Append(card2).ToList();
                }
                else
                {
                    player2 = player2.Append(card2).Append(card1).ToList();
                }

                if (hands.Contains(EncodeHand(player1, player2)))
                {
                    return (1, new List<int>());
                }
                
                hands = hands.Append(EncodeHand(player1, player2)).ToList();
            }

            return player1.Count > player2.Count ? (1, player1) : (2, player2);
        }
    }
}