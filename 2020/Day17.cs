using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using FluentAssertions;

namespace AoC2020
{
    [Command("day17")]
    public class Day17 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\17.txt")
                .ToList();

            var energySource = input
                .SelectMany((l, y) => l
                    .Select((c, x) => (c, cube: new[] {x, y})))
                .Where(t => t.c == '#')
                .Select(t => t.cube)
                .ToHashSet(new CubeComparer())
                .Map(cubes => new EnergySource(
                    new Condition(3, 3),
                    new Condition(2, 3),
                    cubes));

            energySource.BootUp(2).Count.Dump().Should().Be(35);
            energySource.BootUp(3).Count.Dump().Should().Be(213);
            energySource.BootUp(4).Count.Dump().Should().Be(1624);
            energySource.BootUp(5).Count.Dump().Should().Be(9516);

            return default;
        }

        private record Condition(int Min, int Max)
        {
            public bool Meets(int count) => count >= Min && count <= Max;
        };

        private class EnergySource
        {
            private readonly Condition _activate;
            private readonly Condition _remain;
            private readonly HashSet<int[]> _initialActiveCubes;
            private HashSet<int[]> _neighbors;
            private static readonly int[] OneDimNeighbors = {-1, 0, 1};

            public EnergySource(Condition activate, Condition remain, HashSet<int[]> initialActiveCubes)
            {
                _activate = activate;
                _remain = remain;
                _initialActiveCubes = initialActiveCubes;
            }

            public HashSet<int[]> BootUp(int dimensions)
            {
                _neighbors = Enumerable.Range(0, dimensions - 1)
                    .Aggregate(
                        OneDimNeighbors.Select(i => new[] {i}),
                        (accumulator, _) => accumulator.SelectMany(a => OneDimNeighbors.Select(val => a.Append(val).ToArray())))
                    .Where(n => n.Any(i => i != 0))
                    .ToHashSet(new CubeComparer());
                return Enumerable.Range(0, 6).Aggregate(
                    _initialActiveCubes.Select(cube => MoreLinq.MoreEnumerable.ZipLongest(
                        cube,
                        Enumerable.Range(0, dimensions),
                        (i, _) => i).ToArray()).ToHashSet(new CubeComparer()),
                    ExecuteCycle);
            }

            private HashSet<int[]> ExecuteCycle(IReadOnlyCollection<int[]> currentlyActiveCubes, int _)
            {
                var potentials = currentlyActiveCubes.SelectMany(Neighbors).ToList();
                var activate = MeetsCondition(potentials, _activate).ToHashSet(new CubeComparer());
                var remain = MeetsCondition(potentials, _remain).Intersect(currentlyActiveCubes, new CubeComparer())
                    .ToHashSet(new CubeComparer());
                return remain.Union(activate, new CubeComparer()).ToHashSet(new CubeComparer());
            }

            private static IEnumerable<int[]> MeetsCondition(IEnumerable<int[]> cubes, Condition condition) =>
                cubes.GroupBy(c => c, new CubeComparer())
                    .Where(g => condition.Meets(g.Count()))
                    .Select(g => g.Key);

            private List<int[]> Neighbors(int[] cube) =>
                _neighbors
                    .Select(neighbor => MoreLinq.MoreEnumerable
                        .EquiZip(cube, neighbor, (a, b) => a + b)
                        .ToArray())
                    .ToList();
        }

        private class CubeComparer : IEqualityComparer<int[]>
        {
            public bool Equals(int[] x, int[] y) =>
                MoreLinq.MoreEnumerable
                    .EquiZip(x, y, (a, b) => a == b)
                    .All(b => b);

            public int GetHashCode(int[] dimensions) =>
                dimensions
                    .Select((d, idx) => (idx, d))
                    .Aggregate(0, (acc, t) => acc + (int) Math.Pow(100, t.idx) * t.d);
        }
    }
}