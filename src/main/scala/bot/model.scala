package bot

object Dir extends Enumeration {
  type Dir = Value
  val Stay, North, South, East, West = Value
  val allMoves = Seq(North, South, West, East)
}

import Dir._
import bot.Tile.Mine

case class Pos(x: Int, y: Int) {

  def line = x
  def col = y

  def neighbors = Set(North, South, West, East) map to

  def to(dir: Dir) = dir match {
    case Stay  ⇒ this
    case North ⇒ copy(x = x - 1)
    case South ⇒ copy(x = x + 1)
    case East  ⇒ copy(y = y + 1)
    case West  ⇒ copy(y = y - 1)
  }

  def isIn(size: Int) = x >= 0 && x < size && y >= 0 && y < size

  def dist2(to: Pos) = (x - to.x) * (x - to.x) + (y - to.y) * (y - to.y)
}

sealed trait Tile
object Tile {
  case object Air extends Tile
  case object Wall extends Tile
  case object Tavern extends Tile
  case class Hero(id: Int) extends Tile
  case class Mine(heroId: Option[Int]) extends Tile
}

case class Board(size: Int, tiles: Vector[Tile]) {

  def at(pos: Pos): Option[Tile] =
    if (pos isIn size) tiles lift toIndex(pos) else None

  def toIndex(pos: Pos): Int = pos.x * size + pos.y
  def fromIndex(i: Int) = Pos(i / size, i % size)
}

case class Hero(
    id: Int,
    name: String,
    pos: Pos,
    life: Int,
    gold: Int,
    mineCount: Int,
    spawnPos: Pos,
    crashed: Boolean,
    elo: Option[Int]) {

  override def toString = s"Hero $id $pos life:$life mine:$mineCount gold:$gold"
}

case class Game(
    id: String,
    turn: Int,
    maxTurns: Int,
    heroes: List[Hero],
    board: Board,
    finished: Boolean) {

  def hero(id: Int) = heroes.filter(_.id == id).head

}

case class Input(
    game: Game,
    hero: Hero,
    token: String,
    viewUrl: String,
    playUrl: String) {

  def isOpponent(id: Int) = game.hero(id).name != hero.name

  def belongsToFriend(mine: Mine) = mine.heroId.exists(i => !isOpponent(i))

}
