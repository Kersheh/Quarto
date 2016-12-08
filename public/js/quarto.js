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

  /* Quarto game functionality */

  var getBoardWinConditions = (board) => {
    var rows = [], cols = [], diags = [];
    var test = R.forEach((y, i) => {
      return 1;
    }, board);
    console.log(test);
  };

  // check for winning condition
  var isWinner = (board) => {
    getBoardWinConditions(board);
    return false;
  };

  // check board for winner
  var isOver = (board, turn) => {
    if(isWinner(board)) {
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
      // toString to prevent equality bug
      return x.toString() === piece.toString();
    }, pieces);
  };

  // get opponents piece selection and turn
  var getPiece = () => {

  };

  // temporary bot piece selection
  var getRandomPiece = (pieces) => {
    return pieces[Math.floor(Math.random() * pieces.length)];
  };

  // temporary bot piece placement
  var setRandomPiece = (board, piece) => {
    // hacky temporary loop for random bot piece placement
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
    // check for winner
    var winner = isOver(board, turn);
    if(winner.orJust()) {
      displayWinner(winner.just());
    }
    // execute respective player turn
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
        if(!isBoardFull(new_board)) {
          selectTile(runGame, updateBoard, updatePieces, new_board, getRandomPiece(new_pieces), new_pieces, 'p2');
        }
        else {
          runGame(new_board, new_pieces, [], 'p2'); // board is full, end game
        }
      }
      play();
    }
  };

  /* App configuration */

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
