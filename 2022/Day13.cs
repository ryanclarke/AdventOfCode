using System.Text.Json.Nodes;

namespace AoC2022;

public class Day13
{
    private static readonly string InputFilePath = $"{Config.InputRoot}/13i.txt";
    public static void Run()
    {
        var input = File.ReadAllLines(InputFilePath);
        
        var packetPairs = File
            .ReadAllText(InputFilePath)
            .Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
            .Select(pair => pair
                .Split("\n")
                .X(a => (JsonNode.Parse(a[0]), JsonNode.Parse(a[1]))))
            .ToArray();

        Enumerable.Range(0, packetPairs.Length)
            .Where(i =>
            {
                var pair = packetPairs[i];
                var validationResult = Validate(pair.Item1!, pair.Item2!);
                if (validationResult == ValidationResult.Continue) throw new Exception();
                return validationResult == ValidationResult.Good;
            })
            .Sum(x => x + 1)
            .Dump("13a (6568): ");

        var packets = packetPairs.SelectMany(x => new [] {x.Item1, x.Item2}).ToList();
        var divider1 = 1 + packets.Count(node => Validate(node!, JsonNode.Parse("[[2]]")!) == ValidationResult.Good);
        var divider2 = 2 + packets.Count(node => Validate(node!, JsonNode.Parse("[[6]]")!) == ValidationResult.Good);

        (divider1 * divider2).Dump("13b (19493): ");
    }

    private static ValidationResult Validate(JsonNode left, JsonNode right)
    {
        ValidationResult result;
        switch (left)
        {
            case JsonArray l:
                switch (right)
                {
                    case JsonArray r:
                        for (var i = 0; i < l.Count; i++)
                        {
                            if (r.Count <= i) return ValidationResult.Bad;
                            result = Validate(l[i]!, r[i]!);
                            if (result != ValidationResult.Continue) return result;
                        }
                        if (l.Count < r.Count) return ValidationResult.Good;
                        break;
                    case JsonValue r:
                        result = Validate(l, new JsonArray(r.GetValue<int>()));
                        if (result != ValidationResult.Continue) return result;
                        break;
                    default:
                        throw new ArgumentOutOfRangeException(nameof(right), right, null);
                }
                break;
            case JsonValue l:
                switch (right)
                {
                    case JsonArray r:
                        result = Validate(new JsonArray(l.GetValue<int>()), r);
                        if (result != ValidationResult.Continue) return result;
                        break;
                    case JsonValue r:
                        result = (ValidationResult) l.GetValue<int>().CompareTo(r.GetValue<int>());
                        if (result != ValidationResult.Continue) return result;
                        break;
                    default:
                        throw new ArgumentOutOfRangeException(nameof(right), right, null);
                }
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(left), left, null);
        }

        return ValidationResult.Continue;
    }

    private enum ValidationResult
    {
        Good = -1,
        Continue = 0,
        Bad = 1,
    }
}
