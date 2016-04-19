package bot

import scala.util.Random

import bot.Tile._

class DaronaBot extends Bot {
  def move(input: Input) = {
    val me = input.hero.id
    val (myPos: Pos, pos: Pos) = nearestMine(input)

    println(s"mine la plus proche: $pos")
    println(s"mes coords: $myPos")

    val dir = ((pos.y - myPos.y).signum, (pos.x - myPos.x).signum) match {
      case (-1, _) ⇒ Dir.West
      case (1, _)  ⇒ Dir.East
      case (0, 1)  ⇒ Dir.South
      case (0, -1) ⇒ Dir.North
    }

    val dirFallBack = {
      Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
        input.game.board at myPos.to(dir) exists (Wall !=)
      }
    } getOrElse Dir.Stay

    def isPath(t: Tile) = t match {
      case Wall             => false
      case Tavern           => false
      case Mine(Some(`me`)) => false
      case _                => true
    }
    val move = if (input.game.board at myPos.to(dir) exists (isPath)) dir else dirFallBack
    println(move)
    move
  }

  def nearestMine(input: Input): (Pos, Pos) = {
    val notMyMines = mines(input.game.board) filter {
      case (m, p) ⇒
        m.heroId != Some(input.hero.id)
    }

    val myPos = input.hero.pos
    val (mine, pos) = notMyMines minBy { case (m, p) ⇒ p.dist2(myPos) }

    assert((input.game.board at pos).get.isInstanceOf[Mine])

    (myPos, pos)
  }

  def mines(board: Board) = board.tiles.zipWithIndex collect {
    case (m: Mine, i) ⇒ (m, Pos(i / board.size, i % board.size))
  }

}
