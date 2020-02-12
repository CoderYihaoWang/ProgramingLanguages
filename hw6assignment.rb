# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  
  # could use All_My_Pieces = All_Pieces.concat( ... )
  # but the guidelines say we must use the <b> same </b> format
  # which results in longer code, as below
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                   [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z

                  rotations([[0, 0], [-1, 0], [0, 1], [-1, 1], [1, 1]]), # axe
                  [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]], # extra long (only needs two)
                   [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [1, 0]])] # heart

  # your enhancements here
  def self.next_piece (board)
    Piece.new(All_My_Pieces.sample, board)
  end



end

class MyBoard < Board
  # your enhancements here

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end


end

class MyTetris < Tetris
  # your enhancements here

  def initialize
    super.initialize
    @root.bind('c', proc {cheat})
  end

  def cheat
    
  end


  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

end


