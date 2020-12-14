using System;

namespace AoC2020.Utils
{
    public static class Output
    {
        public static T Dump<T>(this T input)
        {
            AocCommand.Console.Output.WriteLine("{0}", input);
            return input;
        }

        public static T Tee<T>(this T input, Action<T> action)
        {
            action(input);
            return input;
        }

        public static T2 Map<T1, T2>(this T1 input, Func<T1, T2> func) => func(input);
    }
}