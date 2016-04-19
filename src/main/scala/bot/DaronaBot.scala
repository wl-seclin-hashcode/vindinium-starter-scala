package bot

import bot.Dir.Dir
import bot.Tile.{ Air, Mine, Wall }

import scala.annotation.tailrec
import scala.util.Random
import bot.Tile.Tavern

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 19/04/16
 * Time: 12:49
 */
class DaronaBot extends Bot {
  def move(input: Input) = {
    val myPos = input.hero.pos
    val board = input.game.board
    val life = input.hero.life
    println(s"life : $life")

    def dirFallBack = Random.shuffle(Dir.allMoves) find { dir ⇒
      !(board at myPos.to(dir) contains Wall)
    } getOrElse Dir.Stay

    def target(pos: Pos) = board at pos exists {
      case Tavern if life <= 60 ⇒ true
      case m: Mine if m.heroId != Some(input.hero.id) && life > 30 ⇒ true
      case _ ⇒ false
    }

    dirToNearest(board, myPos)(target) getOrElse dirFallBack
  }

  def dirToNearest(board: Board, from: Pos)(cond: (Pos ⇒ Boolean)) = {
    val allowedArea: PartialFunction[Tile, Boolean] = {
      case Air          ⇒ true
      case _: Tile.Hero ⇒ true
      case _            ⇒ false
    }

    @tailrec
    def bfs(toVisit: Seq[(Pos, Dir)], visited: IndexedSeq[Boolean]): Option[Dir] =
      if (toVisit.isEmpty) {
        //        println("Not found")
        None
      } else if (cond(toVisit.head._1)) {
        println(s"found: ${toVisit.head._1}, return ${toVisit.head._2}")
        Some(toVisit.head._2)
      } else {
        val (pos, dir) = toVisit.head
        //        println(s"Visiting $pos")
        val moves = if (board at pos exists allowedArea) Dir.allMoves else Seq.empty
        val neighbors = moves map { d ⇒ (pos.to(d), if (dir == Dir.Stay) d else dir) }
        val newVisit = neighbors filter {
          case (p, _) ⇒
            (p isIn board.size) && !visited(board.toIndex(p))
        }
        bfs(toVisit.tail ++ newVisit, visited.updated(board.toIndex(pos), true))
      }

    bfs(Vector((from, Dir.Stay)), Vector.fill(board.size * board.size)(false))
  }

  def mines(board: Board) = board.tiles.zipWithIndex collect {
    case (m: Mine, i) ⇒ (m, board.fromIndex(i))
  }

}
