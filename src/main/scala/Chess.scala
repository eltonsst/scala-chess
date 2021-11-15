import com.whitehatgaming.UserInputFile

type Column = Int
type Row = Int
type Board = Map[Position, Piece]
type Position = (Column, Row) // column and row

sealed trait PieceColor
case object White extends PieceColor
case object Black extends PieceColor

sealed trait PieceType
case object King extends PieceType
case object Queen extends PieceType
case object Rook extends PieceType
case object Bishop extends PieceType
case object Knight extends PieceType
case object Pawn extends PieceType

sealed trait PieceState
case object Init extends PieceState
case object TwoStep extends PieceState
case object Moved extends PieceState

case class Piece(
    pieceType: PieceType,
    pieceColor: PieceColor,
    pieceState: PieceState,
    signature: String
)

case class Player(pieceColor: PieceColor, pieceCaptured: Seq[Piece])

case class State(
    board: Board,
    whitePlayer: Player,
    blackPlayer: Player,
    currentTurn: PieceColor
)

def setPiece(position: Position, piece: Piece, board: Board): Board =
  board + (position -> piece)

def updatePiece(
    update: Piece,
    position: Position,
    board: Board
): Board = board.updatedWith(position)(_ => Some(update))

def getPiece(position: Position, board: Board): Option[Piece] =
  board.get(position)

def dropPiece(position: Position, board: Board): Board = board - position

def movePiece(
    initialPosition: Position,
    finalPosition: Position,
    board: Board
): Option[Board] =
  for {
    piece <- getPiece(initialPosition, board)
    tmpBoard = setPiece(finalPosition, piece, board)
    updBoard = dropPiece(initialPosition, tmpBoard)
  } yield updBoard // 'none' if piece not found

def isValid(
    piece: Piece,
    initialPosition: Position,
    finalPosition: Position,
    state: State
): Boolean =
  val i = piece.pieceColor == state.currentTurn
  val ii = (distance(
    initialPosition,
    finalPosition
  ) > 0)
  val iii = (getPiece(initialPosition, state.board).isDefined)
  val iiii = validMove(
    piece,
    initialPosition,
    finalPosition
  )
  i && ii && iii && iiii

def isMove(
    piece: Piece,
    initialPosition: Position,
    finalPosition: Position,
    state: State
): Boolean =
  val i = isValid(piece, initialPosition, finalPosition, state)
  val ii = isPathClear(
    state.board,
    initialPosition,
    finalPosition,
    piece
  )
  val iii = getPiece(finalPosition, state.board).isEmpty
  val iiii = validMove(
    piece,
    initialPosition,
    finalPosition
  )

  i && ii && iii && iiii

def isCapture(
    piece: Piece,
    initialPosition: Position,
    finalPosition: Position,
    state: State
): Boolean =
  isValid(piece, initialPosition, finalPosition, state) &&
    isPathClear(state.board, initialPosition, finalPosition, piece) &&
    getPiece(finalPosition, state.board).isDefined &&
    getPiece(finalPosition, state.board).get.pieceColor != state.currentTurn

def stepPieceState(
    initialPosition: Position,
    finalPosition: Position,
    piece: Piece
): Piece =
  val dist = distance(initialPosition, finalPosition)
  if (piece.pieceType == Pawn && dist == 2 && piece.pieceState == Init) then
    piece.copy(pieceState = TwoStep)
  else if dist > 0 then piece.copy(pieceState = Moved)
  else piece

// assuming empty board
def validMove(
    piece: Piece,
    initialPosition: Position,
    finalPosition: Position
): Boolean =
  piece.pieceType match {
    case King =>
      Math.abs(finalPosition._1 - initialPosition._1) <= 1 && Math.abs(
        finalPosition._2 - initialPosition._2
      ) <= 1
    case Queen =>
      validMove(
        Piece(Rook, Black, Init, "r"),
        initialPosition,
        finalPosition
      ) || validMove(
        Piece(Bishop, Black, Init, "b"),
        initialPosition,
        finalPosition
      )
    case Rook =>
      finalPosition._1 - initialPosition._1 == 0 || finalPosition._2 - initialPosition._2 == 0
    case Bishop =>
      Math.abs(finalPosition._1 - initialPosition._1) == Math.abs(
        finalPosition._2 - initialPosition._2
      )
    case Knight =>
      val (diffCol, diffRow) = (
        Math.abs(finalPosition._1 - initialPosition._1),
        Math.abs(finalPosition._2 - initialPosition._2)
      )
      (diffCol == 1 && diffRow == 2) || (diffCol == 2 && diffRow == 1)
    case Pawn =>
      if (piece.pieceColor == Black)
        (finalPosition._1 - initialPosition._1) == 0 && ((initialPosition._2 - finalPosition._2) == 1 || (initialPosition._2 - finalPosition._2 == 2 && initialPosition._2 == 7))
      else
        (finalPosition._1 - initialPosition._1) == 0 && ((finalPosition._2 - initialPosition._2) == 1 || (finalPosition._2 - initialPosition._2 == 2 && initialPosition._2 == 2))
  }

def validCapture(
    piece: Piece,
    initialPosition: Position,
    finalPosition: Position
): Boolean =
  piece match {
    case Piece(Pawn, Black, _, _) =>
      (finalPosition._2 - initialPosition._2) == 1 && Math.abs(
        finalPosition._1 - initialPosition._1
      ) == 1
    case Piece(Pawn, White, _, _) =>
      (initialPosition._2 - finalPosition._2) == 1 && Math.abs(
        finalPosition._1 - initialPosition._1
      ) == 1
    case _ => validMove(piece, initialPosition, finalPosition)
  }

def distance(position1: Position, position2: Position): Int =
  Math.max(
    Math.abs(position2._1 - position1._1),
    Math.abs(position2._2 - position1._2)
  )

// check that from position1 to position2 the path is clear
def isPathClear(
    board: Board,
    position1: Position,
    position2: Position,
    piece: Piece
): Boolean =
  val dist = distance(position1, position2)
  val (sc, sr) =
    ((position2._1 - position1._1) / dist, (position2._2 - position1._2) / dist)
  val pieces = (1 to (dist - 1)).map(s =>
    (position1._1 + (sc * s), position1._2 + (sr * s))
  )
  val blockers = pieces.map(p => getPiece(p, board)).filter(_.isDefined)

  if piece.pieceType == Knight then true
  else if dist > 1 then blockers.length == 0
  else true

// assuming king exist for current player
def isCheck(state: State): Boolean =
  val kingPos = state.board
    .filter(p => p._2.pieceType == King && p._2.pieceColor == state.currentTurn)
    .head
  state.board
    .filter(p =>
      (p._2.pieceColor != state.currentTurn) && (isPathClear(
        state.board,
        p._1,
        kingPos._1,
        p._2
      )) && validCapture(p._2, p._1, kingPos._1)
    )
    .nonEmpty

def stepCurrent(state: State): State =
  val opposite = if (state.currentTurn == Black) then White else Black
  state.copy(currentTurn = opposite)

def stepBoard(
    initialPosition: Position,
    finalPosition: Position,
    state: State
): Option[State] =
  val piece = getPiece(initialPosition, state.board)
  state.board match {
    case board
        if piece.isDefined &&
          (isCapture(
            piece.get,
            initialPosition,
            finalPosition,
            state
          ) || isMove(
            piece.get,
            initialPosition,
            finalPosition,
            state
          )) =>
      val newBoard = updatePiece(
        stepPieceState(initialPosition, finalPosition, piece.get),
        initialPosition,
        board
      )
      val ss = movePiece(initialPosition, finalPosition, newBoard).map(b =>
        stepCurrent(state).copy(board = b)
      )
      ss
    case _ => None
  }

def print(state: State): Unit =
  val filesRow = """    a   b   c   d   e   f   e   h"""
  val separator = """  +---+---+---+---+---+---+---+---+"""
  val squares = (1 to 8)

  println(filesRow)
  println(separator)

  squares.reverse.foreach(rank =>
    val line =
      "%d |".format(rank) + squares
        .map(file =>
          " %s |".format(
            getPiece((file, rank), state.board) match {
              case Some(piece) => piece.signature
              case None        => " "
            }
          )
        )
        .reduceLeft(_ + _) + " %d".format(rank)

    println(line)
    println(separator)
  )
  println(filesRow)
  println()

def initialState: State =
  State(
    board = Map[Position, Piece](
      // white first row
      ((1, 1), Piece(Rook, White, Init, "R")),
      ((2, 1), Piece(Knight, White, Init, "N")),
      ((3, 1), Piece(Bishop, White, Init, "B")),
      ((4, 1), Piece(Queen, White, Init, "Q")),
      ((5, 1), Piece(King, White, Init, "K")),
      ((6, 1), Piece(Bishop, White, Init, "B")),
      ((7, 1), Piece(Knight, White, Init, "N")),
      ((8, 1), Piece(Rook, White, Init, "R")),
      // white pawns
      ((1, 2), Piece(Pawn, White, Init, "P")),
      ((2, 2), Piece(Pawn, White, Init, "P")),
      ((3, 2), Piece(Pawn, White, Init, "P")),
      ((4, 2), Piece(Pawn, White, Init, "P")),
      ((5, 2), Piece(Pawn, White, Init, "P")),
      ((6, 2), Piece(Pawn, White, Init, "P")),
      ((7, 2), Piece(Pawn, White, Init, "P")),
      ((8, 2), Piece(Pawn, White, Init, "P")),
      // black first row
      ((1, 8), Piece(Rook, Black, Init, "r")),
      ((2, 8), Piece(Knight, Black, Init, "n")),
      ((3, 8), Piece(Bishop, Black, Init, "b")),
      ((4, 8), Piece(King, Black, Init, "q")),
      ((5, 8), Piece(Queen, Black, Init, "k")),
      ((6, 8), Piece(Bishop, Black, Init, "b")),
      ((7, 8), Piece(Knight, Black, Init, "n")),
      ((8, 8), Piece(Rook, Black, Init, "r")),
      // black pawns
      ((1, 7), Piece(Pawn, Black, Init, "p")),
      ((2, 7), Piece(Pawn, Black, Init, "p")),
      ((3, 7), Piece(Pawn, Black, Init, "p")),
      ((4, 7), Piece(Pawn, Black, Init, "p")),
      ((5, 7), Piece(Pawn, Black, Init, "p")),
      ((6, 7), Piece(Pawn, Black, Init, "p")),
      ((7, 7), Piece(Pawn, Black, Init, "p")),
      ((8, 7), Piece(Pawn, Black, Init, "p"))
    ),
    whitePlayer = Player(White, Nil),
    blackPlayer = Player(Black, Nil),
    currentTurn = White
  )

def readMoves(
    userInput: UserInputFile,
    moves: List[(Position, Position)]
): List[(Position, Position)] =
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

def play(moves: List[(Position, Position)], state: State): Option[State] =
  if moves.isEmpty then Some(state)
  else
    val newState = stepBoard(moves.head._1, moves.head._2, state)
    newState match {
      case Some(s) =>
        println("""  #---#---#---#---#---#---#---#---#  """)
        print(s)
        play(moves.tail, s)
      case None =>
        play(moves.tail, state)
    }
