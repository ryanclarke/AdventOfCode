namespace AoC2021;

public class Day20
{
    public const char LightChar = '█';
    public const char DarkChar = '.';
    public const char LightBit = '1';
    public const char DarkBit = '0';

    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/20.txt")
                .ToList();

        var imageEnhancementAlgorithm = input.First().Select(CharToBit).ToArray();
        var image = input.Skip(2).Select(y => y.Select(CharToBit).ToArray()).ToArray();

        for (int i = 1; i <= 50; i++)
        {
            image = Enhance(image, imageEnhancementAlgorithm, i == 1);
            
            if (i == 2)
            {
                image.Sum(row => row.Count(c => c == LightBit)).Dump("20a (5326): ");
            }
        }
        image.Sum(row => row.Count(c => c == LightBit)).Dump("20b (17096): ");

    }

    private static char[][] PrintImage(char[][] image)
    {
        Console.WriteLine();
        foreach (var row in image)
        {
            Console.WriteLine(row.Select(BitToChar).Stringify());
        }
        Console.WriteLine();
        return image;
    }

    private static char[][] Enhance(char[][] image, char[] imageEnhancementAlgorithm, bool first = false)
    {
        var infiniteBit = first ? DarkBit : image[0][0];
        image = PrepareSurroundingInfinite(image, infiniteBit);

        return Enumerable.Range(0, image.Length)
            .Select(y => Enumerable.Range(0, image[0].Length)
                .Select(x => SquareCoords(x, y)
                        .Select(coord => image.ElementAtOrDefault(coord.Y)?.ElementAtOrDefault(coord.X) ?? infiniteBit)
                        .Select(c => c == '\0' ? infiniteBit : c)
                        .X(bits => Convert.ToInt32(bits.Stringify(), 2))
                        .X(idx => imageEnhancementAlgorithm[idx]))
                .ToArray())
            .ToArray();
    }

    private static char[][] PrepareSurroundingInfinite(char[][] image, char infiniteBit)
    {
        if (image.Any(row => row.First() != infiniteBit))
        {
            image = image.Select(row => row.Prepend(infiniteBit).ToArray()).ToArray();
        }
        image = image.Select(row => row.Prepend(infiniteBit).ToArray()).ToArray();
        if (image.Any(row => row.Last() != infiniteBit))
        {
            image = image.Select(row => row.Append(infiniteBit).ToArray()).ToArray();
        }
        image = image.Select(row => row.Append(infiniteBit).ToArray()).ToArray();

        var infiniteRow = Enumerable.Repeat(infiniteBit, image[0].Length).ToArray();
        if (image.First().Any(i => i != infiniteBit))
        {
            image = image.Prepend(infiniteRow).ToArray();
        }
        image = image.Prepend(infiniteRow).ToArray();
        if (image.Last().Any(i => i != infiniteBit))
        {
            image = image.Append(infiniteRow).ToArray();
        }
        image = image.Append(infiniteRow).ToArray();

        return image;
    }
    
    private static List<(int X, int Y)> SquareCoords(int x, int y)
    {
        return new (int x, int y)[]
        {
                (x - 1, y - 1),
                (x,     y - 1),
                (x + 1, y - 1),
                (x - 1, y),
                (x, y),
                (x + 1, y),
                (x - 1, y + 1),
                (x,     y + 1),
                (x + 1, y + 1)
        }
        .ToList();
    }

    private static char CharToBit(char c) => c switch
    {
        LightChar => LightBit,
        DarkChar => DarkBit,
        '#' => LightBit,
        _ => throw new InvalidDataException($"{nameof(CharToBit)}: {c}")
    };

    private static char BitToChar(char c) => c switch
    {
        LightBit => LightChar,
        DarkBit => DarkChar,
        _ => throw new InvalidDataException($"{nameof(CharToBit)}: {c}")
    };
}
