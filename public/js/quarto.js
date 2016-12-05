'use strict';

var quarto = () => {
  /* Functional features */

  // monet js -- monadic library
  R.Maybe = Maybe;
  R.Either = Either;

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

  /* Utility functions */

  // get list of classes from piece
  var getPieceClasses = (piece) => {
    return R.reduce((x, y) => {
      return x + ' ' + y;
    }, '', piece);
  };

  /* Quarto game functionality */

  // check board for winner
  var isOver = (board) => {
    // return R.Maybe.Just('p1');
    return R.Maybe.Nothing();
  };

  // check if board is empty
  var isBoardEmpty = (board) => {
    var tiles = R.filter((x) => {
      return x.isJust();
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
      // toString to prevent equality bug
      return x.toString() === piece.toString();
    }, pieces);
  };

  // get opponents piece selection and turn
  var getPiece = (pieces) => {

  };

  // temporary bot piece selection
  var getRandomPiece = (pieces) => {
    return pieces[Math.floor(Math.random() * pieces.length)];
  };

  // temporary bot piece placement
  var setRandomPiece = (board, piece) => {
    // hacky loop for random selection
    while(1) {
      var x = Math.floor(Math.random() * 4), y = Math.floor(Math.random() * 4);
      if(board[y][x].isNothing()) {
        return {
          x: x,
          y: y
        };
      }
    }
  };

  // run game
  var runGame = (board, pieces, next_piece, turn) => {
    var winner = isOver(board);
    //
    if(winner.orJust()) {
      console.log(winner.just());
    }
    //
    else {
      // player 1 - user turn
      var play = turn == 'p1' ? () => {
        selectTile(runGame, updateBoard, updatePieces, board, next_piece, pieces, 'p1');
      // player 2 - opponent turn
      } : () => {
        var response = setRandomPiece(board, next_piece, pieces); // opponent turn response
        playPiece(response.x, response.y, next_piece); // update ui
        var new_board = updateBoard(board, next_piece, response.x, response.y);
        var new_pieces = updatePieces(next_piece, pieces);
        selectTile(runGame, updateBoard, updatePieces, new_board, getRandomPiece(new_pieces), new_pieces, 'p2');
      }
      play();
    }
  };

  // connect to IRC
  var connectIRC = () => {
    // establish connection
    // determine who goes first (p1 == user)
    return 'p1';
  };

  // setup app
  var configApp = () => {
    // connect to IRC and determine if user is first or second
    var turn = connectIRC();
    turn ? true : configApp();
    return turn;
  };

  // initialize web app
  var initApp = () => {
    setup(init_pieces); // setup UI
    var turn = configApp();
    runGame(init_board, init_pieces, getRandomPiece(init_pieces), turn); // test with starting piece
  };

  initApp();
};
