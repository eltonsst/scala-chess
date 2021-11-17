import com.whitehatgaming.UserInput
import com.whitehatgaming.UserInputFile
import Chess._

def readMoves(
    userInput: UserInputFile,
    moves: List[((Int, Int), (Int, Int))]
): List[((Int, Int), (Int, Int))] =
  try
    val move = userInput.nextMove()
    val pos1 = (move(0) + 1, 8 - move(1)) // from e2 (file -> 5, rank -> 2)
    val pos2 = (move(2) + 1, 8 - move(3)) // to e4 (file -> 5, rank -> 4)
    readMoves(userInput, moves.appended(pos1, pos2))
  catch
    case _: NullPointerException =>
      moves
    case e: Exception =>
      println(s"error while reading moves: ${e.getMessage}")
      Nil

@main def main: Unit =
  val userInput = new UserInputFile("./data/sample-moves.txt")
  val moves = readMoves(userInput, Nil)
  println(moves) // logging purpose
  play(moves)
