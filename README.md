# taklib
A scala library for the [Tak](http://cheapass.com/tak/) board game

[![Build Status](https://travis-ci.org/Daenyth/taklib.svg?branch=master)](https://travis-ci.org/Daenyth/taklib) [![Coverage Status](https://coveralls.io/repos/github/Daenyth/taklib/badge.svg?branch=master)](https://coveralls.io/github/Daenyth/taklib?branch=master) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/ae261facd8f5421dbe9a895f4bcdcd58)](https://www.codacy.com/app/Daenyth/taklib)

## Example usage

```scala
import com.github.daenyth.taklib._
```

```scala
val invalid: Either[String, Game] = Game.fromTps("invalid")
// invalid: Either[String,com.github.daenyth.taklib.Game] = Left(`[' expected but `i' found)
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
- Create new games with an empty board
- Create a new game from a TPS string
- Create a (potentially completed) game from a PTN file, including games beginning with TPS tags
- Play moves that are checked for validity
- Detect all game-ending conditions
- Run a rudimentary interactive mode on the command line
- Add your own custom game rules
- Run an HTTP REST server that accepts tps and move input, returning tps.

## Interactive game on the command line
```
sbt takcli/run
```

## TPS Server
```
sbt tpsserver/run
```

The server runs on localhost:8080 and listens for POST requests to `/tpsMove` with a json payload like
```json
{
  "tps" : "x6/x6/x6/x6/x6/x6 1 1",
  "move" : "a1"
}
```

For example:
```python
# python
import requests
requests.post("http://localhost:8080/tpsMove", json={
  "tps" : "x6/x6/x6/x6/x6/x6 1 1",
  "move" : "a1"
}).json()
# {u'OkMove': {u'nextState': u'1,x,x,x,x,x/x,x,x,x,x,x/x,x,x,x,x,x/x,x,x,x,x,x/x,x,x,x,x,x/x,x,x,x,x,x 2 1'}}

requests.post("http://localhost:8080/tpsMove", json={
  "tps" : "invalid",
  "move" : "a1"
}).json()
# {u'errors': [u"TPS: string matching regex `(1|2)+[SC]?' expected but `i' found"]}

requests.post("http://localhost:8080/tpsMove", json={
  "tps" : "1,1,1,1,1,x/x6/x6/x6/x6/x6 1 1",
  "move" : "a6"
}).json()
# {u'GameOver': {u'finalState': u'1,1,1,1,1,1/x,x,x,x,x,x/x,x,x,x,x,x/x,x,x,x,x,x/x,x,x,x,x,x/x,x,x,x,x,x 2 1',
#   u'result': {u'RoadWin': {u'player': {u'White': {}}}}}}
```

The exact json structure is likely going to be changed. Right now it's automatically derived from the internal representation, rather than being designed to be easy to use directly.

## Add custom rules
Create a RuleSet to make new games with

```
scala> Game.ofSize(7, DefaultRules)
res0: Either[String,com.github.daenyth.taklib.Game] = scala.Left(Bad game size: 7)

// A new variant with a size-7 board that has 40 flatstones and 7 capstones per player!
scala> Game.ofSize(7, new RuleSet {
     | val rules = DefaultRules.rules
     | val expectedStoneColor = DefaultRules.expectedStoneColor
     | val stoneCounts = DefaultRules.stoneCounts + ((7, (40, 7)))
     | })
res1: Either[String,com.github.daenyth.taklib.Game] = scala.Right(com.github.daenyth.taklib.Game@517564bf)
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
- Scalaz usage. Taklib will only support cats

## Testing

`sbt test`

## License

Taklib is under the AGPL3. For more information, see [COPYING](COPYING)
