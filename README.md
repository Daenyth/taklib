# taklib
A scala library for the [Tak](http://cheapass.com/tak/) board game

## Example usage

```scala
import com.github.daenyth.taklib._
```

```scala
val invalid: String \/ Game = Game.fromTps("invalid")
// invalid: scalaz.\/[String,com.github.daenyth.taklib.Game] = -\/(`[' expected but `i' found)
val game = Game.fromTps("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1 12 2 ]").getOrElse(throw new Exception)
// game: com.github.daenyth.taklib.Game = com.github.daenyth.taklib.Game@78c4cfdd
val winner = game.winner
// winner: Option[com.github.daenyth.taklib.GameEndResult] = Some(FlatWin(White))
```

```scala
val game = Game.ofSize(6).getOrElse(throw new Exception())
// game: com.github.daenyth.taklib.Game = com.github.daenyth.taklib.Game@5cf72de5
val winner = game.winner
// winner: Option[com.github.daenyth.taklib.GameEndResult] = None
val next: MoveResult[Game] = game.takeTurn(PlayFlat(Black, BoardIndex(1, 1)))
// next: com.github.daenyth.taklib.MoveResult[com.github.daenyth.taklib.Game] = OkMove(com.github.daenyth.taklib.Game@66f6a349)
```

## Release status
Taklib is currently alpha status - there may be bugs and the api is not stable yet

## What you can do now
- Create games with empty boards or boards from TPS
- Play moves that are checked for validity
- Detect all game-ending conditions
- Run a rudimentary interactive mode on the command line

## Interactive game on the command line
```
sbt takcli/run
```

## Goals
- Easy to use
- Type safe interface
- Thread safe - taklib is fully based off immutable data structures, so is inherantly thread safe
- Able to support branching history with arbitrary rollback/rollforward (not implemented yet)

## Non-goals
- Not aiming to be the fastest runtime - I'm not benchmarking anything until the project is much more stable.
- Stable API - for now. This is a new library, and so the api can change without notice as I find better ways to do things.
- Supporting scala.js - for now. It should be possible with little effort but it's not a priority. Patches welcome
- Cats support. Taklib will use scalaz only for the near future

## Testing

`sbt test`

## License

Taklib is under the GPL3. For more information, see [COPYING](COPYING)
