namespace AoC2021;

public class Day11
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/11.txt")
                .SelectMany((line, y) => line.Select((c, x) => (Coord: (x, y), Energy: int.Parse(c.ToString()))))
                .ToArray();

        var octopuses = input.ToDictionary(a => a.Coord, a => a.Energy);
        var flashes = 0;
        var step = 0;
        do
        {
            step++;

            for (int y = 0; y < 10; y++)
            {
                for (int x = 0; x < 10; x++)
                {
                    IncreaseEnergy((x, y));
                }
            }

            for (int y = 0; y < 10; y++)
            {
                for (int x = 0; x < 10; x++)
                {
                    octopuses[(x, y)] = octopuses[(x, y)] >= 10 ? 0 : octopuses[(x, y)];
                }
            }

            //Console.WriteLine(step);
            //for (int y = 0; y < 10; y++)
            //{
            //    for (int x = 0; x < 10; x++)
            //    {
            //        Console.Write(octopuses[(x,y)]);
            //    }
            //    Console.WriteLine();
            //}

            if (step == 100)
            {
                flashes.Dump("11a (1673): ");
            }
        } while (!octopuses.All(o => o.Value == 0));
        step.Dump("11b (279): ");

        void IncreaseEnergy((int, int) coord)
        {
            octopuses[coord] += 1;
            if (octopuses[coord] == 10)
            {
                flashes++;
                var neighbors = Neighbors(coord);
                neighbors.ForEach(neighbor => IncreaseEnergy(neighbor));
            }
        }

        List<(int x, int y)> Neighbors((int, int) coord)
        {
            var (x, y) = coord;
            return new (int x, int y)[]
            {
                (x - 1, y - 1),
                (x,     y - 1),
                (x + 1, y - 1),
                (x - 1, y),
                (x + 1, y),
                (x - 1, y + 1),
                (x,     y + 1),
                (x + 1, y + 1)
            }
            .Where(t => t.x is >=0 and <=9 && t.y is >=0 and <= 9)
            .ToList();
        }
    }
}
