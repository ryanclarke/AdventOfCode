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
    }
}