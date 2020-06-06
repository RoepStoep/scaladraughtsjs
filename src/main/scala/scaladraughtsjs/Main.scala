package scaladraughtsjs

import scala.scalajs.js.JSApp
import scala.scalajs.js
import org.scalajs.dom
import js.Dynamic.{ /* global => g, newInstance => jsnew, */ literal => jsobj }
import js.JSConverters._
import js.annotation._

import draughts.{ Success, Failure, DraughtsGame => Game, Pos, Role, PromotableRole, Replay, Status, Move }
import draughts.variant.Variant
import draughts.format.{ UciCharPair, Uci }
import draughts.format.pdn.Reader

object Main extends JSApp {
  def main(): Unit = {

    val self = js.Dynamic.global

    self.addEventListener("message", { e: dom.MessageEvent =>

      try {
        val data = e.data.asInstanceOf[Message]
        val reqidOpt = data.reqid.asInstanceOf[js.UndefOr[String]].toOption
        val payload = data.payload.asInstanceOf[js.Dynamic]
        val fen = payload.fen.asInstanceOf[js.UndefOr[String]].toOption
        val variantKey = payload.variant.asInstanceOf[js.UndefOr[String]].toOption
        val variant = variantKey.flatMap(Variant(_))

        data.topic match {

          case "init" => {
            init(reqidOpt, variant, fen)
          }
          case "dests" => {
            val path = payload.path.asInstanceOf[js.UndefOr[String]].toOption
            val uci = payload.uci.asInstanceOf[js.UndefOr[String]].toOption
            val fullCapture = payload.fullCapture.asInstanceOf[js.UndefOr[Boolean]].toOption.getOrElse(false)
            val move = uci.flatMap(Uci.Move.apply)
            fen.fold {
              sendError(reqidOpt, data.topic, "fen field is required for dests topic")
            } { fen =>
              getDests(reqidOpt, variant, fen, path, move, fullCapture)
            }
          }
          case "situation" => {
            val path = payload.path.asInstanceOf[js.UndefOr[String]].toOption
            val uci = payload.uci.asInstanceOf[js.UndefOr[String]].toOption
            val move = uci.flatMap(Uci.Move.apply)
            fen.fold {
              sendError(reqidOpt, data.topic, "fen field is required for situation topic")
            } { fen =>
              val game = Game(variant, Some(fen))
              self.postMessage(Message(
                reqid = reqidOpt,
                topic = "situation",
                payload = jsobj(
                  "situation" -> gameSituation(game, lastMoveUci = move),
                  "path" -> path.orUndefined
                )
              ))

              ()
            }
          }
          case "threefoldTest" => {
            val pdnMoves = payload.pdnMoves.asInstanceOf[js.Array[String]].toList
            val initialFen = payload.initialFen.asInstanceOf[js.UndefOr[String]].toOption
            val finalSquare = payload.finalSquare.asInstanceOf[js.UndefOr[Boolean]].toOption
            Replay(pdnMoves, initialFen, variant getOrElse Variant.default, finalSquare.getOrElse(false)) match {
              case Success(Reader.Result.Complete(replay)) => {
                self.postMessage(Message(
                  reqid = reqidOpt,
                  topic = "threefoldTest",
                  payload = jsobj(
                    "threefoldRepetition" -> replay.state.board.history.threefoldRepetition,
                    "status" -> jsobj(
                      "id" -> Status.Draw.id,
                      "name" -> Status.Draw.name
                    )
                  )
                ))
              }
              case Success(Reader.Result.Incomplete(_, errors)) => sendError(reqidOpt, data.topic, errors.head)
              case Failure(errors) => sendError(reqidOpt, data.topic, errors.head)
            }
          }
          case "move" => {
            val promotion = payload.promotion.asInstanceOf[js.UndefOr[String]].toOption
            val origS = payload.orig.asInstanceOf[String]
            val destS = payload.dest.asInstanceOf[String]
            val pdnMovesOpt = payload.pdnMoves.asInstanceOf[js.UndefOr[js.Array[String]]].toOption
            val uciMovesOpt = payload.uciMoves.asInstanceOf[js.UndefOr[js.Array[String]]].toOption
            val pdnMoves = pdnMovesOpt.map(_.toVector).getOrElse(Vector.empty[String])
            val uciMoves = uciMovesOpt.map(_.toList).getOrElse(List.empty[String])
            val path = payload.path.asInstanceOf[js.UndefOr[String]].toOption
            val uci = payload.uci.asInstanceOf[js.UndefOr[String]].toOption
            (for {
              orig <- Pos.posAt(origS)
              dest <- Pos.posAt(destS)
              fen <- fen
            } yield (orig, dest, fen)) match {
              case Some((orig, dest, fen)) =>
                move(reqidOpt, variant, fen, pdnMoves, uciMoves, orig, dest, Role.promotable(promotion), path, uci)
              case None =>
                sendError(reqidOpt, data.topic, s"step topic params: $origS, $destS, $fen are not valid")
            }
          }
          case "pdnDump" => {
            val pdnMoves = payload.pdnMoves.asInstanceOf[js.Array[String]].toList
            val initialFen = payload.initialFen.asInstanceOf[js.UndefOr[String]].toOption
            val white = payload.white.asInstanceOf[js.UndefOr[String]].toOption
            val black = payload.black.asInstanceOf[js.UndefOr[String]].toOption
            val date = payload.date.asInstanceOf[js.UndefOr[String]].toOption
            val finalSquare = payload.finalSquare.asInstanceOf[js.UndefOr[Boolean]].toOption
            Replay(pdnMoves, initialFen, variant getOrElse Variant.default, finalSquare.getOrElse(false)) match {
              case Success(Reader.Result.Complete(replay)) => {
                val pdn = PdnDump(replay.state, initialFen, replay.setup.startedAtTurn + 1, white, black, date)
                self.postMessage(Message(
                  reqid = reqidOpt,
                  topic = "pdnDump",
                  payload = jsobj(
                    "pdn" -> pdn.toString
                  )
                ))
              }
              case Success(Reader.Result.Incomplete(_, errors)) => sendError(reqidOpt, data.topic, errors.head)
              case Failure(errors) => sendError(reqidOpt, data.topic, errors.head)
            }
          }
          case _ => {
            sendError(reqidOpt, data.topic, "Invalid command.")
          }
        }
      } catch {
        case ex: Exception => {
          val data = e.data.asInstanceOf[Message]
          val reqidOpt = data.reqid.asInstanceOf[js.UndefOr[String]].toOption
          sendError(reqidOpt, data.topic, "Exception caught in scaladraughtsjs: " + ex)
        }
      }
    })

    def init(reqid: Option[String], variant: Option[Variant], fen: Option[String]): Unit = {
      val game = Game(variant, fen)
      self.postMessage(Message(
        reqid = reqid,
        topic = "init",
        payload = jsobj(
          "variant" -> new VariantInfo {
            val key = game.board.variant.key
            val name = game.board.variant.name
            val shortName = game.board.variant.shortName
            val title = game.board.variant.title
          },
          "setup" -> gameSituation(game)
        )
      ))

      ()
    }

    def getDests(reqid: Option[String], variant: Option[Variant], fen: String, path: Option[String], lastMoveUci: Option[Uci.Move], fullCapture: Boolean): Unit = {
      val game = Game(variant, Some(fen))
      val movable = !game.situation.end
      val dests = if (movable) possibleDests(game, lastMoveUci.map(_.dest), fullCapture) else emptyDests
      self.postMessage(Message(
        reqid = reqid,
        topic = "dests",
        payload = jsobj(
          "dests" -> dests,
          "path" -> path.orUndefined
        )
      ))

      ()
    }

    def move(reqid: Option[String], variant: Option[Variant], fen: String, pdnMoves: Vector[String], uciMoves: List[String], orig: Pos, dest: Pos, promotion: Option[PromotableRole], path: Option[String], uci: Option[String]): Unit = {
      val captures = uci.flatMap(Uci.Move.apply).flatMap(_.capture)
      Game(variant, Some(fen))(orig, dest, promotion, draughts.MoveMetrics(), captures.isDefined, captures) match {
        case Success((newGame, move)) => {
          self.postMessage(Message(
            reqid = reqid,
            topic = "move",
            payload = jsobj(
              "situation" -> gameSituation(newGame.withPdnMoves(pdnMoves ++ newGame.pdnMoves), Some(move), uciMoves, promotion),
              "path" -> path.orUndefined
            )
          ))

          ()
        }
        case Failure(errors) => sendError(reqid, "move", errors.head)
      }
    }

    def sendError(reqid: Option[String], callerTopic: String, error: String): Unit = {
      self.postMessage(Message(
        reqid = reqid,
        topic = "error",
        payload = jsobj(
          "callerTopic" -> callerTopic,
          "error" -> error
        )
      ))

      ()
    }
  }

  private val emptyDests: js.Dictionary[js.Array[String]] = js.Dictionary()

  private def moveOrDropToUciCharPair(m: Move): UciCharPair =
    UciCharPair(m.toUci)

  private def gameSituation(
    game: Game,
    lastMoveOpt: Option[Move] = None,
    prevUciMoves: List[String] = List.empty[String],
    promotionRole: Option[PromotableRole] = None,
    lastMoveUci: Option[Uci.Move] = None,
  ): js.Object = {

    val lmUci = lastMoveOpt.map(m => if (game.situation.ghosts == 0) m.toUci.uci else m.toShortUci.uci)
    val lmDest = lastMoveOpt.fold(lastMoveUci.map(_.dest))(m => Some(m.dest))

    val mergedUciMoves = lmUci.fold(prevUciMoves) { uci =>
      prevUciMoves :+ uci
    }
    val movable = !game.situation.end

    new Situation {
      val id = lastMoveOpt.fold("")(moveOrDropToUciCharPair(_).toString)
      val variant = game.board.variant.key
      val fen = draughts.format.Forsyth >> game
      val player = game.player.name
      val dests = if (movable) possibleDests(game, lmDest, false) else emptyDests
      val drops = possibleDrops(game)
      val captureLength = getCaptureLength(game, lmDest)
      val end = game.situation.end
      val playable = game.situation.playable(true)
      val winner = game.situation.winner.map(_.name).orUndefined
      val kingMoves = (if (game.board.variant.key == "frisian" || game.board.variant.key == "frysk") {
        val whiteKingMoves = game.board.history.kingMoves.whiteKing.map(_.key).getOrElse("00")
        val blackKingMoves = game.board.history.kingMoves.blackKing.map(_.key).getOrElse("00")
        Some(jsobj(
          "white" -> game.board.history.kingMoves.white,
          "black" -> game.board.history.kingMoves.black,
          "whiteKing" -> whiteKingMoves,
          "blackKing" -> blackKingMoves
        ))
      } else None).orUndefined
      val uci = lmUci.orUndefined
      val san = game.pdnMoves.lastOption.orUndefined
      val pdnMoves = game.pdnMoves.toJSArray
      val uciMoves = mergedUciMoves.toJSArray
      val promotion = promotionRole.map(_.forsyth).map(_.toString).orUndefined
      val status = game.situation.status.map { s =>
        jsobj(
          "id" -> s.id,
          "name" -> s.name
          )
      }.orUndefined
      val ply = game.turns
    }
  }

  private def getCaptureLength(game: Game, lastDestOpt: Option[Pos]): js.UndefOr[Int] = {
    val captLen = if (game.situation.ghosts > 0) {
      val dest = if (lastDestOpt.isDefined) lastDestOpt
      else if (game.pdnMoves.isEmpty) None
      else {
        val move = game.pdnMoves(game.pdnMoves.length - 1)
        val sep = move.lastIndexOf('x')
        if (sep == -1) None
        else draughts.Pos.posAt(move.substring(sep + 1))
      }
      dest match {
        case Some(dest) => game.situation.captureLengthFrom(dest)
        case _ => game.situation.allMovesCaptureLength
      }
    } else
      game.situation.allMovesCaptureLength
    captLen.orUndefined
  }

  private def possibleDests(game: Game, lastDestOpt: Option[Pos], fullCapture: Boolean): js.Dictionary[js.Array[String]] = {
    val dests = if (game.situation.ghosts > 0) {
      val dest = if (lastDestOpt.isDefined) lastDestOpt
      else if (game.pdnMoves.isEmpty) None
      else {
        val move = game.pdnMoves(game.pdnMoves.length - 1)
        val sep = move.lastIndexOf('x')
        if (sep == -1) None
        else draughts.Pos.posAt(move.substring(sep + 1))
      }
      dest match {
        case Some(dest) => Map(dest -> game.situation.destinationsFrom(dest))
        case _ => game.situation.allDestinations
      }
    } else if (fullCapture) {
      game.situation.allDestinationsFinal
    } else {
      game.situation.allDestinations
    }
    dests.map {
      case (pos, dests) => (pos.toString -> dests.map(_.toString).toJSArray)
    }.toJSDictionary
  }

  private def possibleDrops(game: Game): js.UndefOr[js.Array[String]] = {
    game.situation.drops.map { drops =>
      drops.map(_.toString).toJSArray
    }.orUndefined
  }
}

@js.native
trait Message extends js.Object {
  val topic: String
  val payload: js.Any
  val reqid: js.UndefOr[String]
}

object Message {
  def apply(topic: String, payload: js.Any, reqid: Option[String]): Message =
    js.Dynamic.literal(topic = topic, payload = payload, reqid = reqid.orUndefined).asInstanceOf[Message]
}

@ScalaJSDefined
trait VariantInfo extends js.Object {
  val key: String
  val name: String
  val shortName: String
  val title: String
}

@ScalaJSDefined
trait Situation extends js.Object {
  val id: String
  val ply: Int
  val variant: String
  val fen: String
  val player: String
  val dests: js.Dictionary[js.Array[String]]
  val drops: js.UndefOr[js.Array[String]]
  val captureLength: js.UndefOr[Int]
  val end: Boolean
  val playable: Boolean
  val status: js.UndefOr[js.Object]
  val winner: js.UndefOr[String]
  val kingMoves: js.UndefOr[js.Object]
  val pdnMoves: js.Array[String]
  val uciMoves: js.Array[String]
  val san: js.UndefOr[String]
  val uci: js.UndefOr[String]
  val promotion: js.UndefOr[String]
}
