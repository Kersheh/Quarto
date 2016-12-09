'use strict';

/* Functional features */

// monet js -- monadic library
R.Maybe = Maybe;

/* Quarto game setup */

// piece characteristics
var piece_height = ['tall', 'short'];
var piece_top = ['holey', 'unholey'];
var piece_colour = ['dark', 'light'];
var piece_shape = ['box', 'cylinder'];

// cartesian product of list of lists
var cartesianProduct = (list) => {
  return R.reduce((a, b) => {
    return R.unnest(R.map((x) => {
      return R.map((y) => {
        return R.concat(x, [y]);
      }, b);
    }, a));
  }, [[]], list);
};

// pre-generated list of all pieces
var init_pieces = cartesianProduct([piece_height, piece_top, piece_colour, piece_shape]);

// initial empty board where each tile is Nothing
var init_board = R.repeat(R.repeat(R.Maybe.Nothing(), 4), 4);

/* Quarto game functionality */

// retrieve rows or columns from board with 4 pieces
var getLines = (board) => {
  var lines = R.reject((i) => {
    return i.length === 0;
  }, R.map((y) => {
    return R.reject((x) => {
      return x.isNothing();
    }, y);
  }, board));
  return R.filter((line) => {
    return line.length === 4;
  }, lines);
};

// retrieve diagonal line from top left to bottom right of board
var getDiagonal = (board) => {
  var diag = R.unnest(board.map((y, i) => {
    return y.filter((x, j) => {
      if(i === j) {
        return x;
      }
    });
  }));
  var line = R.reject((i) => {
    return i.isNothing();
  }, diag);
  if(line.length === 4) {
    return line;
  }
  return [];
};

// rotate the board 90 degrees
var rotateBoard = (board) => {
  return R.map((row) => {
    return R.reverse(row);
  }, R.transpose(board, board));
}

// retrieve both diagonals of the board
var getDiagonals = (board) => {
  var lines = [getDiagonal(board), getDiagonal(rotateBoard(board))];
  return R.filter((line) => {
    return line.length === 4;
  }, lines);
};

// retrieve properties of pieces
var getProperties = (pieces) => {
  return R.unnest(R.map((i) => {
    return i.orJust();
  }, R.unnest(pieces)));
};

// check for winning condition of a line
var checkLine = (line) => {
  var props = R.countBy((x) => {
    return x;
  })(getProperties(line));
  var count = R.filter((prop) => {
    return prop === 4;
  }, R.values(props));
  if(count.length > 0) {
    return true;
  }
  return false;
};

// check for winning board states
var isWinner = (lines) => {
  var results = R.map(checkLine, lines);
  return R.contains(true, results);
};

// check every applicable row, column, and diagonal for a winning line
var checkBoard = (board) => {
  var rows = getLines(board);
  var cols = getLines(R.transpose(board, board));
  var diags = getDiagonals(board);
  if(isWinner(rows) || isWinner(cols) || isWinner(diags)) {
    return true;
  }
  return false;
};

// check board for winner
var isOver = (board, turn) => {
  if(checkBoard(board)) {
    if(turn === 'p1') {
      return R.Maybe.Just('p2');
    }
    return R.Maybe.Just('p1');
  }
  if(isBoardFull(board)) {
    return R.Maybe.Just('tie');
  }
  return R.Maybe.Nothing();
};

// check if board is empty
var isBoardEmpty = (board) => {
  var tiles = R.filter((x) => {
    return x.isJust();
  }, R.flatten(board));
  return tiles.length === 0;
};

// check if board is full
var isBoardFull = (board) => {
  var tiles = R.filter((x) => {
    return x.isNothing();
  }, R.flatten(board));
  return tiles.length === 0;
};

// update board with piece
var updateBoard = (board, piece, x, y) => {
  return R.update(y, R.update(x, R.Maybe.Just(piece), R.nth(y, board)), board);
};

// update list of available pieces
var updatePieces = (piece, pieces) => {
  return R.reject((x) => {
    return x.toString() === piece.toString(); // toString to prevent equality bug
  }, pieces);
};

// send opponent finished turn
var sendTurn = () => {

};

// get opponents turn
var waitTurn = () => {

};

// temporary bot piece selection
var getRandomPiece = (pieces) => {
  return pieces[Math.floor(Math.random() * pieces.length)];
};

// temporary bot turn
var botResponse = (board, piece, pieces) => {
  if(piece.length === 0) {
    return {
      x: -1,
      y: -1,
      next_piece: getRandomPiece(pieces)
    };
  }
  // hacky temporary loop for random bot piece placement
  while(1) {
    var x = Math.floor(Math.random() * 4), y = Math.floor(Math.random() * 4);
    if(board[y][x].isNothing()) {
      return {
        x: x,
        y: y,
        next_piece: getRandomPiece(updatePieces(piece, pieces))
      };
    }
  }
};

// run game
var runGame = (board, pieces, next_piece, turn, first) => {
  // check for winner
  var winner = isOver(board, turn);
  if(winner.orJust()) {
    displayWinner(winner.just());
  }
  // execute respective player turn
  else {
    // player 1 - user turn
    var play = turn == 'p1' ? () => {
      selectTile(board, next_piece, pieces, 'p1', first);
    // player 2 - opponent turn
    } : () => {
      var response = botResponse(board, next_piece, pieces); // opponent turn response
      if(response.x >= 0 && response.y >= 0) {
        playPiece(response.x, response.y, next_piece); // update ui
        var new_board = updateBoard(board, next_piece, response.x, response.y);
        var new_pieces = updatePieces(next_piece, pieces);
      }
      else {
        var new_board = board;
        var new_pieces = pieces;
      }

      if(!isBoardFull(new_board)) {
        selectTile(new_board, response.next_piece, new_pieces, 'p2', first);
      }
      else {
        runGame(new_board, new_pieces, [], 'p2', false); // board is full, end game
      }
    }
    play();
  }
};

/* App configuration */

// connect to IRC
var connectIRC = (callback) => {
  // establish connection
  // determine who goes first (p1 == user)
  var test = setInterval(() => {
    callback('p1');
    clearInterval(test);
  }, 1000);
  test;
};

// initialize web app
var initApp = () => {
  setup(init_pieces); // setup UI
  connectIRC((turn) => {
    runGame(init_board, init_pieces, [], turn, true);
  });
};
