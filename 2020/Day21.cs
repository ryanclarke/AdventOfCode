using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using AoC2020.Utils;
using CliFx.Attributes;
using FluentAssertions;
// ReSharper disable StringLiteralTypo

namespace AoC2020
{
    [Command("day21")]
    public class Day21 : AocCommand
    {
        protected override ValueTask Run()
        {
            var input = File
                .ReadAllLines(@"C:\dev\AdventOfCode\2020\input\21.txt")
                .ToList();

            var foods = input
                .Select(line => line
                    .Replace(")", string.Empty)
                    .Split(" (contains ")
                    .Map(a => (
                        allergens: a[1].Split(", "),
                        ingredients: a[0].Split(" "))))
                .ToList();

            var allergens = foods
                .SelectMany(f => f.allergens
                    .Select(a => (allergen: a, f.ingredients)))
                .GroupBy(t => t.allergen)
                .Select(g => (
                    allergen: g.Key,
                    ingredientLists: g.Select(t => t.ingredients).ToList()))
                .Select(a => (
                    a.allergen,
                    potentiallyBadIngredients: a
                        .ingredientLists
                        .SelectMany(x => x)
                        .GroupBy(x => x)
                        .Where(ig => a.ingredientLists.Count == ig.Count())
                        .Select(ig => ig.Key)
                        .ToList()))
                .ToList();
                
            foods
                .SelectMany(f => f.ingredients)
                .Map(x => x)
                .Where(ingredient => !allergens
                    .SelectMany(a => a.potentiallyBadIngredients)
                    .Contains(ingredient))
                .Map(x => x)
                .ToList()
                .Count()
                .Dump().Should().Be(2265);


            var pairs = new List<(string allergen, string ingredient)> { };
            var remainingAllergens = allergens;
            while (pairs.Count < allergens.Count)
            {
                bool NotDiscovered(string i) => !pairs.Select(p => p.ingredient).ToList().Contains(i);
                var newlyDiscovered = remainingAllergens
                    .OrderBy(a => a.potentiallyBadIngredients.Count)
                    .First(a => a.potentiallyBadIngredients.Count() == 1);
                pairs = pairs
                    .Append(newlyDiscovered
                        .Map(t => (
                            t.allergen,
                            ingredient: t.potentiallyBadIngredients.Single(NotDiscovered))))
                    .ToList();
                remainingAllergens = remainingAllergens
                    .Select(a => (a.allergen, ingredient: a.potentiallyBadIngredients.Where(NotDiscovered).ToList()))
                    .ToList();
            }
            
            pairs
                .OrderBy(a => a.allergen)
                .Map(l => string.Join(',', l.Select(a => a.ingredient).ToList()))
                .Dump().Should().Be("dtb,zgk,pxr,cqnl,xkclg,xtzh,jpnv,lsvlx");
            
            return default;
        }
    }
}