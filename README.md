# taklib
A scala library for the [Tak](cheapass.com/tak/) board game

## Example usage

```scala
import com.github.daenyth.taklib._
```

```scala
val invalid: String \/ Game = Game.fromTps("invalid")
// invalid: scalaz.\/[String,com.github.daenyth.taklib.Game] = -\/(`[' expected but `i' found)
val game = Game.fromTps("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1 12 2 ]").getOrElse(throw new Exception)
// game: com.github.daenyth.taklib.Game = Game(5,25,...
val winner = game.winner
// winner: Option[com.github.daenyth.taklib.GameEndResult] = Some(FlatWin(White))
```

```scala
val game = Game.ofSize(6)
// game: com.github.daenyth.taklib.Game = Game(6,1,...
val winner = game.winner
// winner: Option[com.github.daenyth.taklib.GameEndResult] = None
val next: InvalidMove.type \/ Game = game.takeTurn(PlayFlat(Black, BoardIndex(1, 1)))
// next: com.github.daenyth.taklib.Board.Checked[com.github.daenyth.taklib.Game] = \/-(Game(6,1,
```

## Release status
Taklib is currently alpha status - there may be bugs and the api is not stable yet

## What you can do now
- Create games with empty boards or boards from TPS
- Play moves that are checked for validity
- Detect all game-ending conditions

## Goals
- Easy to use
- Type safe interface
- Thread safe - taklib is fully based off immutable data structures, so is inherantly thread safe
- Able to support branching history with arbitrary rollback/rollforward (not implemented yet)

## Non-goals
- Not aiming to be the fastest runtime - I'm not benchmarking anything until the project is much more stable.
- Stable API - for now. This is a new library, and so the api can change without notice as I find better ways to do things.
- Supporting scala.js - for now. If all my dependencies support it, I can do this later
- Cats support. Taklib will use scalaz only for the near future

## Testing

`sbt test`

## License

Taklib is under the GPL3. For more information, see [COPYING](COPYING)
