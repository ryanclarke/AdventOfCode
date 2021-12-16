namespace AoC2021;

public static class Day16
{
    public static void Run()
    {
        var input = File
                .ReadAllLines("../../../input/16.txt")
                .First();

        //ParseTransmission("8A004A801A8002F478").λ(VersionsSum).Dump("16: ");
        //ParseTransmission("620080001611562C8802118E34").λ(VersionsSum).Dump("12: ");
        //ParseTransmission("C0015000016115A2E0802F182340").λ(VersionsSum).Dump("23: ");
        //ParseTransmission("A0016C880162017C3686B18A3D4780").λ(VersionsSum).Dump("31: ");
        ParseTransmission(input).X(VersionsSum).Dump("16a (996): ");

        //ParseTransmission("C200B40A82").λ(Solve).Dump("3: ");
        //ParseTransmission("04005AC33890").λ(Solve).Dump("54: ");
        //ParseTransmission("880086C3E88112").λ(Solve).Dump("7: ");
        //ParseTransmission("CE00C43D881120").λ(Solve).Dump("9: ");
        //ParseTransmission("D8005AC2A8F0").λ(Solve).Dump("1: ");
        //ParseTransmission("F600BC2D8F").λ(Solve).Dump("0: ");
        //ParseTransmission("9C005AC2F8F0").λ(Solve).Dump("0: ");
        //ParseTransmission("9C0141080250320F1802104A08").λ(Solve).Dump("1: ");
        ParseTransmission(input).X(Solve).Dump("16b (96257984154): ");
    }

    private static Packet ParseTransmission(string transmission)
    {
        var bits = ToBits(transmission);
        var ptr = 0;

        return ParsePacket();

        Packet ParsePacket()
        {
            var version = Version();
            var typeId = TypeId();
            if (typeId == 4)
            {
                return new Packet.LiteralValue(version, LiteralValue());
            }
            else
            {
                var lengthTypeId = LengthTypeId();
                var packets = new List<Packet>();
                if (lengthTypeId == 0)
                {
                    var lengthOfSubpackets = Segment(15);
                    var endOfSubpackets = ptr + lengthOfSubpackets;
                    while (ptr < endOfSubpackets)
                    {
                        packets.Add(ParsePacket());
                    }
                }
                else
                {
                    var countOfSubpackets = Segment(11);
                    for (var i = 0; i < countOfSubpackets; i++)
                    {
                        packets.Add(ParsePacket());
                    }
                }
                return new Packet.Operator(version, typeId, packets);
            }
        }

        int Version() => Segment(3);
        int TypeId() => Segment(3);
        int LengthTypeId() => Segment(1);
        long LiteralValue()
        {
            var value = "";
            var segment = "";
            do
            {
                segment = BitsSegment(5);
                value += segment[1..];
            } while (segment[0] == '1');
            return Convert.ToInt64(value, 2);
        }
        string BitsSegment(int count) => bits!.Skip(ptr).Take(count).Stringify().T(_ => ptr += count);
        int Segment(int count) => BitsSegment(count).X(s => Convert.ToInt32(s, 2));
    }

    private static long VersionsSum(Packet p) => p switch
    {
        Packet.LiteralValue literal => literal.Version,
        Packet.Operator o => o.Version + o.Subpackets.Sum(VersionsSum),
        _ => throw new InvalidDataException()
    };

    private static long Solve(Packet p) => p switch
    {
        Packet.LiteralValue literal => literal.Value,
        Packet.Operator o => o.TypeId switch
        {
            0 => o.Subpackets.Sum(Solve),
            1 => o.Subpackets.Aggregate(1L, (a, x) => a * Solve(x)),
            2 => o.Subpackets.Select(Solve).Min(),
            3 => o.Subpackets.Select(Solve).Max(),
            5 => o.Subpackets.Select(Solve).X(ps => ps.First() > ps.Last() ? 1L : 0L),
            6 => o.Subpackets.Select(Solve).X(ps => ps.First() < ps.Last() ? 1L : 0L),
            7 => o.Subpackets.Select(Solve).X(ps => ps.First() == ps.Last() ? 1L : 0L),
            _ => throw new ArgumentException(nameof(o.TypeId))
        },
        _ => throw new InvalidDataException()
    };

    public record Packet(int Version, int TypeId)
    {
        public record Operator(int Version, int TypeId, List<Packet> Subpackets) : Packet(Version, TypeId);
        public record LiteralValue(int Version, long Value) : Packet(Version, 4);
    }

    private static string ToBits(string s) => s.SelectMany(c => BitsConversion[c]).Stringify();

    private static Dictionary<char, string> BitsConversion = new()
    {
        { '0', "0000" },
        { '1', "0001" },
        { '2', "0010" },
        { '3', "0011" },
        { '4', "0100" },
        { '5', "0101" },
        { '6', "0110" },
        { '7', "0111" },
        { '8', "1000" },
        { '9', "1001" },
        { 'A', "1010" },
        { 'B', "1011" },
        { 'C', "1100" },
        { 'D', "1101" },
        { 'E', "1110" },
        { 'F', "1111" }
    };
}
