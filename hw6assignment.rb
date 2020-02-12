# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  
  All_My_Pieces = All_Pieces.concat(
    [rotations([[0, 0], [-1, 0], [0, 1], [-1, 1], [1, 1]]), # axe
     [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]], # extra long (only needs two)
      [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
     rotations([[0, 0], [0, -1], [1, 0]])]) # heart

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
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

  # rotate the piece for 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

end

class MyTetris < Tetris
  # your enhancements here

  def key_bindings
    super
    @root.bind('c', proc {self.cheat})
    @root.bind('u', proc {@board.rotate_180})
  end

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def cheat
    if @running
      @running = false
      @timer.stop
    else
      @running = true
      self.run_game
    end
  end

  

end