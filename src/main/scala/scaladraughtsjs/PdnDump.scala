package scaladraughtsjs

import draughts.format.pdn.{ Pdn, Tag, Tags }
import draughts.format.{ pdn => draughtsPdn }
import draughts.{ DraughtsGame => Game }

import scala.scalajs.js
import js.Dynamic.{ global => g, newInstance => jsnew }

object PdnDump {

  def apply(
    game: Game,
    initialFen: Option[String],
    startedAtTurn: Int,
    white: Option[String] = None,
    black: Option[String] = None,
    date: Option[String] = None): Pdn = {
    val ts = tags(game, initialFen, white, black, date)
    Pdn(ts, turns(game.pdnMoves, startedAtTurn))
  }

  private def tags(
    game: Game,
    initialFen: Option[String],
    white: Option[String] = None,
    black: Option[String] = None,
    date: Option[String] = None): Tags = {
      val d = jsnew(g.Date)()
      Tags(List(
        Tag(_.Event, "Casual Game"),
        Tag(_.Site, "https://lidraughts.org"),
        Tag(_.Date, date getOrElse d.toLocaleString()),
        Tag(_.White, white getOrElse "Anonymous"),
        Tag(_.Black, black getOrElse "Anonymous"),
        Tag(_.Result, result(game)),
        Tag("PlyCount", game.turns),
        Tag(_.FEN, initialFen getOrElse "?"),
        Tag(_.Variant, game.board.variant.name.capitalize),
        Tag(_.Termination, game.situation.status.fold("?")(s => s.name))
      ))
  }

  private def turns(moves: Vector[String], from: Int): List[draughtsPdn.Turn] =
    (moves grouped 2).zipWithIndex.toList map {
      case (moves, index) => draughtsPdn.Turn(
        number = index + from,
        white = moves.headOption.filter(".." !=).map(s => draughtsPdn.Move(s, draughts.White)),
        black = moves.lift(1).map(s => draughtsPdn.Move(s, draughts.Black)))
    } filterNot (_.isEmpty)

  private def result(game: Game) = game.situation.status.fold("*") { _ =>
    game.situation.winner.fold("1-1")(c => c.fold("2-0", "0-2"))
  }
}
