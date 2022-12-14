using System.Numerics;

namespace AoC2022;

public class Day11
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/11.txt";
    public static void Run()
    {
        var input = File
                .ReadAllLines(InputFilePath)
                .ToList();

        var testMonkeys = new Monkey[]
        {
            new(0, new Queue<BigInteger>(new BigInteger[] {79, 98}), x => x * 19, x => x % 23 == 0 ? 2 : 3),
            new(1, new Queue<BigInteger>(new BigInteger[] {54, 65, 75, 74}), x => x + 6, x => x % 19 == 0 ? 2 : 0),
            new(2, new Queue<BigInteger>(new BigInteger[] {79, 60, 97}), x => x * x, x => x % 13 == 0 ? 1 : 3),
            new(3, new Queue<BigInteger>(new BigInteger[] {74}), x => x + 3, x => x % 17 == 0 ? 0 : 1)
        }; 
        
        var monkeys = new Monkey[]
        {
            new(0, new Queue<BigInteger>(new BigInteger[] {54, 61, 97, 63, 74}), x => x * 7, x => x % 17 == 0 ? 5 : 3),
            new(1, new Queue<BigInteger>(new BigInteger[] {61, 70, 97, 64, 99, 83, 52, 87}), x => x + 8, x => x % 2 == 0 ? 7 : 6),
            new(2, new Queue<BigInteger>(new BigInteger[] {60, 67, 80, 65}), x => x * 13, x => x % 5 == 0 ? 1 : 6),
            new(3, new Queue<BigInteger>(new BigInteger[] {61, 70, 76, 69, 82, 56}), x => x + 7, x => x % 3 == 0 ? 5 : 2),
            new(4, new Queue<BigInteger>(new BigInteger[] {79, 98}), x => x + 2, x => x % 7 == 0 ? 0 : 3),
            new(5, new Queue<BigInteger>(new BigInteger[] {72, 79, 55}), x => x + 1, x => x % 13 == 0 ? 2 : 1),
            new(6, new Queue<BigInteger>(new BigInteger[] {63}), x => x + 4, x => x % 19 == 0 ? 7 : 4),
            new(7, new Queue<BigInteger>(new BigInteger[] {72, 51, 93, 63, 80, 86, 81}), x => x * x, x => x % 11 == 0 ? 0 : 4)
        };

        // monkeys = testMonkeys;
        for (var round = 0; round < 10000; round++)
        {
            foreach (var monkey in monkeys)
            {
                while (monkey.Items.Any())
                {
                    var item = monkey.Items.Dequeue();
                    item = monkey.Process(item);
                    var throwTo = monkey.Test(item);
                    monkeys[throwTo].Items.Enqueue(item);
                }

            }

            // if (round == 999)
            // {
            //     round.Dump();
            // }
            // round.Dump();
        }

        monkeys
            .Select(m => m.Inspections())
            .OrderDescending()
            .Take(2)
            .ToArray()
            .X(l => l.First() * l.Last())
            // .Dump("11a (50172): ");
            .Dump("11b (11614682178): ");

    }

    private record Monkey(int Id, Queue<BigInteger> Items, Func<BigInteger, BigInteger> Operation, Func<BigInteger, int> Test)
    {
        private BigInteger inspections = 0;
        public BigInteger Inspections() => inspections; 

        public BigInteger Process(BigInteger item)
        {
            inspections++;
            return Operation(item) % (17 * 2 * 5 * 3 * 7 * 13 * 19 * 11);
        }
    }
}
