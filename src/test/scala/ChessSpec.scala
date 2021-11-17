import org.scalatest.matchers.should.Matchers
import Chess._
import org.scalatest.wordspec.AnyWordSpec

class ChessSpec extends AnyWordSpec with Matchers {

  "ScalaChess" should {
    "start with the pieces in the correct places" in {
      val board = initialState.board
      val whitePawns = board
        .filterKeys(pos => pos._2 == 2)
        .values
        .filter(p => p.pieceType == Pawn && p.pieceColor == White)
        .toList
      val blackPawns = board
        .filterKeys(pos => pos._2 == 7)
        .values
        .filter(p => p.pieceType == Pawn && p.pieceColor == Black)
        .toList
      whitePawns.length shouldBe 8
      blackPawns.length shouldBe 8
    }

    "change the player turn correctly" in {
      val s = initialState
      stepCurrent(s).currentColor shouldNot be(s.currentColor)
    }

    "find if there is a check" in {
      val s = State(
        board = Map[Position, Piece](
          ((5, 1), Piece(King, White, Moved, "K")),
          ((5, 6), Piece(Queen, Black, Moved, "q"))
        ),
        currentColor = White
      )
      isCheck(s) shouldBe true
    }

    "find that there is no check" in {
      val s = State(
        board = Map[Position, Piece](
          ((5, 1), Piece(King, White, Moved, "K")),
          ((6, 6), Piece(Queen, Black, Moved, "q"))
        ),
        currentColor = White
      )
      isCheck(s) shouldBe false
    }

    "return none for an invalid move after a check" in {
      val s = State(
        board = Map[Position, Piece](
          ((5, 1), Piece(King, White, Moved, "K")),
          ((5, 6), Piece(Queen, Black, Moved, "q"))
        ),
        currentColor = White
      )
      stepBoard((5, 1), (5, 2), s) shouldBe None
    }

    "return a new state after escaping to a check" in {
      val s = State(
        board = Map[Position, Piece](
          ((5, 1), Piece(King, White, Moved, "K")),
          ((5, 6), Piece(Queen, Black, Moved, "q"))
        ),
        currentColor = White
      )
      stepBoard((5, 1), (4, 1), s) shouldNot be(None)
    }

    "move a pawn by two if it was never touched" in {
      val s = State(
        board = Map[Position, Piece](
          ((5, 1), Piece(King, White, Moved, "K")),
          ((5, 2), Piece(Pawn, White, Init, "P"))
        ),
        currentColor = White
      )
      stepBoard((5, 2), (5, 4), s) shouldNot be(None)
    }

    "not move a pawn by two if it was touched" in {
      val s = State(
        board = Map[Position, Piece](
          ((5, 1), Piece(King, White, Moved, "K")),
          ((5, 3), Piece(Pawn, White, Moved, "P"))
        ),
        currentColor = White
      )
      stepBoard((5, 2), (5, 4), s) shouldBe None
    }
  }
}
