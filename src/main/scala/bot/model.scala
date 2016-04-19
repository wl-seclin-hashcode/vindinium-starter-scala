package bot

object Dir extends Enumeration {
  type Dir = Value
  val Stay, North, South, East, West = Value
}

import Dir._

case class Pos(line: Int, col: Int) {

  def neighbors = Set(North, South, West, East) map to

  def to(dir: Dir) = dir match {
    case Stay  ⇒ this
    case North ⇒ copy(line = line - 1)
    case South ⇒ copy(line = line + 1)
    case East  ⇒ copy(col = col + 1)
    case West  ⇒ copy(col = col - 1)
  }

  def isIn(size: Int) = line >= 0 && line < size && col >= 0 && col < size

  def dist2(to: Pos) = (line - to.line)*(line - to.line) + (col - to.col)*(col - to.col)
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
    if (pos isIn size) tiles lift (pos.line * size + pos.col) else None
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
  finished: Boolean)

case class Input(
  game: Game,
  hero: Hero,
  token: String,
  viewUrl: String,
  playUrl: String)
