using System.Threading.Tasks;
using CliFx;

namespace AoC2020
{
    public abstract class AocCommand : ICommand
    {
        public static IConsole Console { get; private set; }
        
        public ValueTask ExecuteAsync(IConsole console)
        {
            Console = console;
            return Run();
        }

        protected abstract ValueTask Run();
    }
}