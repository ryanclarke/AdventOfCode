public static class Extensions
{
    public static T T<T>(this T input, Action<T> action)
    {
        action(input);
        return input;
    }

    public static T2 X<T1, T2>(this T1 input, Func<T1, T2> func) => func(input);

    public static T Dump<T>(this T input, string label = "")
    {
        Console.WriteLine($"{label}{input}");
        return input;
    }

    public static string Stringify(this IEnumerable<char> chars) => new(chars.ToArray());
    public static string JoinStrings(this IEnumerable<string> strings, string separator = "") => separator == "" ? string.Concat(strings) : string.Join(separator, strings);
    public static string JoinLines(this IEnumerable<string> strings, int newLines = 1) => strings.JoinStrings(Environment.NewLine.Times(newLines));
    public static string Times(this string s, int i) => Enumerable.Repeat(s, i).JoinStrings();
    public static string[] Arrayify(this string a) => a.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries & StringSplitOptions.TrimEntries);

    public static int Multiply(this IEnumerable<int> ints) => ints.Aggregate(1, (a, b) => a * b);
    public static long Multiply(this IEnumerable<long> longs) => longs.Aggregate(1L, (a, b) => a * b);
}
