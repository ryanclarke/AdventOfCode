namespace AoC2021;

public class Day24
{
    //99999999489978 - 12080
    //99989999489978 - 12079
    //99979999489978 - 12078
    //99969999489978 - 12077
    //99963949489978 - 464
    //98799999489978 - 463
    //97963949489978 - 17
    //87963949489978 - 16
    //77963949489978 - 15
    //67963949489978 - 14
    //57963949489978 - 13
    //47963949489978 - 12
    //37963949489978 - 11
    //27963949489978 - 10
    //17963949489978 - 9
    //11111111111111 - 2950451649
    //11111111111171 - 113478909
    //11111111112171 - 4364573
    //11111111112671 - 167868
    //11111116112661 - 6463
    //11111116112671 - 6456
    //11111616112721 - 249
    //11111616112722 - 9
    public static void Run()
    {
        //long serialNumber = 99_999_999_489_978;
        //serialNumber = 11_111_111_111_111;
        //long lowestZ = long.MaxValue;

        //do
        //{
        //    var z = IsValid(serialNumber);

        //    if (z < lowestZ)
        //    {
        //        z.Dump($"{serialNumber} - ");
        //        lowestZ = z;
        //    }

        //    do
        //    {
        //        serialNumber += 1; // _000_000;
        //    } while (serialNumber.ToString().Contains('0'));
        //} while (lowestZ != 0);

        //(serialNumber + 1).Dump();

        var valids = GetValids();
        var max = valids.Max().Dump();
    }

    public static long[] GetValids()
    {
        var layerVars = new (int A, int B, int C)[]
        {
            ( 11,  1,  8),
            ( 14,  1, 13),
            ( 10,  1,  2),
            (  0, 26,  7),
            ( 12,  1, 11),
            ( 12,  1,  4),
            ( 12,  1, 13),
            ( -8, 26, 13),
            ( -9, 26, 10),
            ( 11,  1,  1),
            (  0, 26,  2),
            ( -5, 26, 14),
            ( -6, 26,  6),
            (-12, 26, 14)
        };

        (bool X, long Z) LayerFunc(int layer, long z, int w)
        {
            var (a, b, c) = layerVars[layer];
            var x = z % 26 + a == w;
            return (x || b == 1, x
                ? z / b
                : z / b * 26 + w + c);
        }


        var digits = Enumerable.Range(1, 9).Reverse();


        var _1 = RunLayer(new Dictionary<long, long> { { 0, 0 } }, 0);
        var _2 = RunLayer(_1, 1);
        var _3 = RunLayer(_2, 2);
        var _4 = RunLayer(_3, 3);
        var _5 = RunLayer(_4, 4);
        var _6 = RunLayer(_5, 5);
        var _7 = RunLayer(_6, 6);
        var _8 = RunLayer(_7, 7);
        var _9 = RunLayer(_8, 8);
        var _10 = RunLayer(_9, 9);
        var _11 = RunLayer(_10, 10);
        var _12 = RunLayer(_11, 11);
        var _13 = RunLayer(_12, 12);
        var _14 = RunLayer(_13, 13);
        var count = _14.Values.Distinct().Count();
        var min = _14.Values.Min();

        return Array.Empty<long>();

        Dictionary<long, long> RunLayer(Dictionary<long, long> layers, int layerNumber)
        {
            return layers.SelectMany(x => digits.Select(d => (Mem: LayerFunc(layerNumber, x.Key, d), Num: x.Value * 10 + d))).Where(t => t.Mem.X).GroupBy(t => t.Mem.Z).ToDictionary(g => g.Key, g => g.MinBy(t => t.Num).Num);
        }
    }

    public static long IsValid(long serialNumber)
    {
        var i = 0;
        var sn = serialNumber.ToString().Select(c => c - 48).ToArray();

        long w = 0;
        long x = 0;
        long y = 0;
        long z = 0;

        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 1;
        x += 11;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 8;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 1;
        x += 14;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 13;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 1;
        x += 10;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 2;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 26;
        x += 0;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 7;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 1;
        x += 12;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 11;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 1;
        x += 12;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 4;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 1;
        x += 12;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 13;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 26;
        x += -8;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 13;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 26;
        x += -9;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 10;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 1;
        x += 11;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 1;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 26;
        x += 0;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 2;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 26;
        x += -5;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 14;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 26;
        x += -6;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 6;
        y *= x;
        z += y;
        w = sn[i++];
        x *= 0;
        x += z;
        x %= 26;
        z /= 26;
        x += -12;
        x = x == w ? 1 : 0;
        x = x == 0 ? 1 : 0;
        y *= 0;
        y += 25;
        y *= x;
        y += 1;
        z *= y;
        y *= 0;
        y += w;
        y += 14;
        y *= x;
        z += y;

        return z;
    }
}
