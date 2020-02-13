# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# Author : Yihao Wang
# Date   : 14/2/2020

class MyPiece < Piece
  
  # add three new pieces
  All_My_Pieces = All_Pieces + [
    rotations([[0, 0], [-1, 0], [0, 1], [-1, 1], [1, 1]]), # axe
    [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],           # extra long (only needs two)
     [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
    rotations([[0, 0], [0, -1], [1, 0]])                   # heart
  ]                 

  # use All_My_Pieces instead of My_Pieces
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # get the cheat piece
  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end

end # MyPiece

class MyBoard < Board

  # use MyPiece class instead of Piece
  # a field @cheated is added to keep track of whether a cheat has been called
  def initialize (game)
    super game
    @current_block = MyPiece.next_piece(self)
    @cheated = false
  end

  # get the next MyPiece
  def next_piece
    if !@cheated
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = MyPiece.cheat_piece(self)
      @cheated = false
    end
    @current_pos = nil
  end

  # rotate the piece for 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if !@cheated and @score >= 100
      @score -= 100
      @cheated = true
    end
  end

  # fix the store_current in super to handle variable-size blocks
  # otherwise it will cause an error when drop down is pressed when there is a heart piece
  # and may cause 5-pieces not being rendered properly after a drop down
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0...(locations.size)).each{|index| # this line is changed
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end # MyBoard

class MyTetris < Tetris

  # add two key bindings
  def key_bindings
    super
    @root.bind('c', proc {@board.cheat})
    @root.bind('u', proc {@board.rotate_180})
  end
  
  # naturally, when two keys are added, there should be two additional buttons
  # though this is not required
  def buttons
    super
    rotate_180 = TetrisButton.new('rotate', 'lightgreen'){@board.rotate_180}
    rotate_180.place(35, 50, 27, 571)
    
    cheat = TetrisButton.new('cheat', 'coral'){@board.cheat}
    cheat.place(35, 50, 127, 571)
  end

  # use MyBoard instead of Board
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

end # MyTetris