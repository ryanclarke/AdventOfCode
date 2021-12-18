namespace AoC2021;

public class Day18
{
    const int LBracket = -1;
    const int RBracket = -3;
    const int Comma = -100;

    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/18.txt")
                .ToList();
        var arrayed = input.Select(Arrayify).ToList();

        //TestPart1();
        arrayed.X(Sum).X(Magnitude).Dump("18a (4088): ");

        //TestPart2();
        arrayed.X(LargestMagnitude).Dump("18b (4536): ");
    }

    public record Result(bool Changed, int[] Number);

    static int[] Arrayify(string s) => s
            .Select(c => c switch
            {
                '[' => LBracket,
                ']' => RBracket,
                ',' => Comma,
                _ => c.ToString().X(int.Parse)
            })
            .ToArray();

    public static string ReSnailfish(int[] n) => n
        .Select(i => i switch
        {
            Comma => ",",
            LBracket => "[",
            RBracket => "]",
            _ => i.ToString()
        })
        .JoinStrings();

    static long LargestMagnitude(List<int[]> ns)
    {
        long max = 0;

        for (int a = 0; a < ns.Count; a++)
        {
            for (int b = 0; b < ns.Count; b++)
            {
                if (a != b)
                {
                    var mag = Sum(new List<int[]> { ns[a], ns[b] }).X(Magnitude);

                    max = Math.Max(max, mag);
                }
            }
        }

        return max;
    }

    static long Magnitude(int[] n)
    {
        var stack = new Stack<long>();
        for (int i = 0; i < n.Length; i++)
        {
            switch (n[i])
            {
                case int v when v >= 0: stack.Push(v); break;
                case RBracket: stack.Push(stack.Pop() * 2 + stack.Pop() * 3); break;
                default: break;
            };
        }
        return stack.Single();
    }

    static int[] Sum(List<int[]> ns)
    {
        return ns.Skip(1).Aggregate(ns.First(), Add);
    }

    static int[] Add(int[] first, int[] second)
    {
        return first.Prepend(LBracket).Append(Comma).Concat(second).Append(RBracket).ToArray().X(Reduce).Number;
    }

    static Result Reduce(int[] n)
    {
        var changed = false;
        while (true)
        {
            var e = Explode(n);
            if (e.Changed)
            {
                changed = true;
                n = e.Number;
                continue;
            }
            else
            {
                var r = Split(n);
                if (r.Changed)
                {
                    changed = true;
                    n = r.Number;
                    continue;
                }
                else
                {
                    return new Result(changed, n);
                }
            }
        }
    }

    static Result Explode(int[] n)
    {
        var depth = 0;
        var lastInt = new Stack<int>();
        lastInt.Push(-10);
        var rightInt = int.MaxValue;
        var explodeDiscovered = false;
        for (int i = 0; i < n.Length; i++)
        {
            if (n[i] == Comma)
            {
                // do nothing
            }
            else if (n[i] == LBracket || n[i] == RBracket)
            {
                depth += n[i] + 2;
            }
            else
            {
                if (explodeDiscovered)
                {
                    rightInt = i;
                    break;
                }

                if (depth > 4 && lastInt.Peek() == i - 2)
                {
                    explodeDiscovered = true;
                }
                lastInt.Push(i);
            }
        }

        if (explodeDiscovered)
        {
            var rightValue = lastInt.Pop();
            var leftValue = lastInt.Pop();
            var left = lastInt.Pop();

            if (rightInt < n.Length)
            {
                n[rightInt] += n[rightValue];
            }
            if (left >= 0)
            {
                n[left] += n[leftValue];
            }

            n = n.Take(leftValue - 1).Append(0).Concat(n.Skip(rightValue + 2)).ToArray();
        }

        return new Result(explodeDiscovered, n);
    }

    static Result Split(int[] n)
    {
        for (int i = 0; i < n.Length; i++)
        {
            if (n[i] >= 10)
            {
                var half = n[i] / 2.0;
                var a = (int)Math.Floor(half);
                var b = (int)Math.Ceiling(half);
                var newN = n.Take(i).Concat(new int[] { LBracket, a, Comma, b, RBracket }).Concat(n.Skip(i + 1)).ToArray();
                return new Result(true, newN);
            }
        }
        return new Result(false, n);
    }

    static void TestPart1()
    {
        Check(Explode, "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]");
        Check(Explode, "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]");
        Check(Explode, "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]");
        Check(Explode, "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]");
        Check(Explode, "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]");
        Check(Reduce, "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]");
        CheckSum(@"[1,1]
[2,2]
[3,3]
[4,4]", "[[[[1,1],[2,2]],[3,3]],[4,4]]");
        CheckSum(@"[1,1]
[2,2]
[3,3]
[4,4]
[5,5]", "[[[[3,0],[5,3]],[4,4]],[5,5]]");
        CheckSum(@"[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]", "[[[[5,0],[7,4]],[5,5]],[6,6]]");
        CheckSum(@"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]", "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]");
        CheckSingleMagnitude("[9,1]", 29L);
        CheckSingleMagnitude("[1,9]", 21L);
        CheckSingleMagnitude("[[9,1],[1,9]]", 129L);
        CheckSingleMagnitude("[[1,2],[[3,4],5]]", 143L);
        CheckSingleMagnitude("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384L);
        CheckSingleMagnitude("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445L);
        CheckSingleMagnitude("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791L);
        CheckSingleMagnitude("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137L);
        CheckSingleMagnitude("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488L);
        CheckMagnitude(@"[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]", 4140L);

        void CheckMagnitude(string given, long expected)
        {
            var actual = Sum(given.Split("\r\n").Select(Arrayify).ToList()).X(Magnitude);
            Console.WriteLine($"{actual == expected} {given}");
            Console.WriteLine($"{actual,10}");
            Console.WriteLine($"{expected,10}");
        }

        void CheckSingleMagnitude(string given, long expected)
        {
            var actual = given.X(Arrayify).X(Magnitude);
            Console.WriteLine($"{actual == expected} {given}");
            Console.WriteLine($"{actual,10}");
            Console.WriteLine($"{expected,10}");
        }

        void CheckSum(string given, string expected)
        {
            var actual = Sum(given.Split("\r\n").Select(Arrayify).ToList()).X(ReSnailfish);
            Console.WriteLine($"{actual == expected}");
            Console.WriteLine($"      {actual}");
            Console.WriteLine($"      {expected}");
        }

        void Check(Func<int[], Result> func, string given, string expected)
        {
            var actual = given.X(Arrayify).X(func).Number.X(ReSnailfish);
            var b = (actual == expected).ToString();
            Console.WriteLine($"{b}: {given}");
            Console.WriteLine($"      {actual}");
            Console.WriteLine($"      {expected}");
        }
    }

    static void TestPart2()
    {
        LargestMagnitude(@"[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]".Split("\r\n").Select(Arrayify).ToList()).T(n => Console.WriteLine($"{n == 3993L} {n} 3993"));
    }
}
