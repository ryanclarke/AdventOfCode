namespace AoC2021;

public class Day21
{
    public const int BoardPositions = 10;
    public static void Run()
    {
        //Part1.PlayGame(4, 8).Dump("21a sample (739785): ");
        Part1.PlayGame(8, 5).Dump("21a (597600): ");
        //Part2.PlayGame(4, 8).Dump("21b sample (444356092776315): ");
        Part2.PlayGame(8, 5).Dump("21b (634769613696613): ");
    }

    public class Part1
    {
        public static int PlayGame(int startPlayer1, int startPlayer2)
        {
            const int winningScore = 1000;

            var die = new DeterministicDie(1000);
            var players = new[] { new PlayerState(startPlayer1, 0), new PlayerState(startPlayer2, 0) };

            for (var turn = 0; players.All(p => p.Score < winningScore); turn++)
            {
                var playerNumber = turn % 2;
                var move = die.SumOfNextThreeRolls();
                var newPosition = (players[playerNumber].Position + move).Moduloop(BoardPositions);
                var newScore = players[playerNumber].Score + newPosition;
                players[playerNumber] = new PlayerState(newPosition, newScore);
            }

            return players.Select(p => p.Score).Min() * die.NumberOfRolls;
        }

        public record DeterministicDie(int Sides)
        {
            public int NumberOfRolls = 0;
            public int NextRoll()
            {
                NumberOfRolls++;
                return NumberOfRolls.Moduloop(Sides);
            }
            public int SumOfNextThreeRolls() => NextRoll() + NextRoll() + NextRoll();
        }
        public record PlayerState(int Position, int Score);
    }

    public class Part2
    {
        private static List<Dist> DiracDist = new();
        public static long PlayGame(int startPlayer1, int startPlayer2)
        {
            var _123 = Enumerable.Range(1, 3);
             DiracDist = _123.SelectMany(a => _123.SelectMany(b => _123.Select(c => a + b + c)))
                .GroupBy(i => i)
                .Select(g => new Dist(g.Key, g.Count()))
                .ToList();

            var results = TakeTurn(new PlayerState(startPlayer1, 0), new PlayerState(startPlayer2, 0), 0, 1);

            return Math.Max(results.P1Wins, results.P2Wins);
        }

        public static (long P1Wins, long P2Wins) TakeTurn(PlayerState p1, PlayerState p2, int turn, long numberOfGames)
        {
            var wins = (0L, 0L);
            foreach (var dist in DiracDist)
            {
                if (turn % 2 == 0)
                {
                    var newPosition = (p1.Position + dist.Roll).Moduloop(BoardPositions);
                    var newScore = p1.Score + newPosition;
                    if (newScore >= 21)
                    {
                        wins = Add(wins, (numberOfGames * dist.Times, 0));
                    }
                    else
                    {
                        wins = Add(wins, TakeTurn(new PlayerState(newPosition, newScore), p2, turn + 1, numberOfGames * dist.Times));
                    }
                }
                else
                {
                    var newPosition = (p2.Position + dist.Roll).Moduloop(BoardPositions);
                    var newScore = p2.Score + newPosition;
                    if (newScore >= 21)
                    {
                        wins = Add(wins, (0, numberOfGames * dist.Times));
                    }
                    else
                    {
                        wins = Add(wins, TakeTurn(p1, new PlayerState(newPosition, newScore), turn + 1, numberOfGames * dist.Times));
                    }
                }
            }

            return wins;
        }

        public static (long P1Wins, long P2Wins) Add((long P1Wins, long P2Wins) a, (long P1Wins, long P2Wins) b) => (a.P1Wins + b.P1Wins, a.P2Wins + b.P2Wins);

        public record Dist(int Roll, int Times);
        public record PlayerState(int Position, int Score);
    }
}
