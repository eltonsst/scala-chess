import com.whitehatgaming.UserInput
import com.whitehatgaming.UserInputFile

@main def hello: Unit =
  val init = initialState
  print(init)
  val userInput = new UserInputFile("./data/sample-moves-invalid.txt")
  val moves = readMoves(userInput, Nil)
  println(moves)
  play(moves, init)
