'use strict';

$(() => {
  var modules = () => {
    return (typeof R !== 'undefined' &&        // ramda js -- functional library
            typeof Maybe !== 'undefined' &&    // monet js -- Maybe monad
            typeof Either !== 'undefined' &&   // monet js -- Either monad
            typeof UI !== 'undefined');        // web app UI with jQuery
  };

  if(!modules()) {
    console.log('Modules missing.');
  }
  else {
    // monet js -- monadic library
    R.Maybe = Maybe;
    R.Either = Either;

    // jQuery tile selectors
    var getTileSelector = (x, y) => {
      return $('tr[y="' + y + '"]').children('td[x="' + x + '"]');
    };
    var curriedGetTiles = R.curry(getTileSelector);
    var tiles_selectors = R.ap(R.map(curriedGetTiles, [0, 1, 2, 3]), [0, 1, 2, 3]);

    // jQuery pieces selectors
    var pieces_selectors = [$('#p0'), $('#p1'), $('#p2'), $('#p3'),
                            $('#p4'), $('#p5'), $('#p6'), $('#p7'),
                            $('#p8'), $('#p9'), $('#p10'), $('#p11'),
                            $('#p12'), $('#p13'), $('#p14'), $('#p15')];

    // initial empty board where each tile is Nothing
    var init_board = R.repeat(R.repeat(R.Maybe.Nothing(), 4), 4);

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

    var getPieceClasses = (piece) => {
      return R.reduce((x, y) => {
        return x + ' ' + y;
      }, '', piece);
    };

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
    }

    // update board with piece
    var updateBoard = (board, piece, x, y) => {
      return R.update(y, R.update(x, R.Maybe.Just(piece), R.nth(y, board)), board);
    };

    // update list of available pieces
    var updatePieces = (piece, pieces) => {
      return R.reject((x) => {
        return x == piece;
      }, pieces);
    };

    // select piece for opponent
    var selectPiece = () => {

    };

    // get opponents piece selection and turn
    var getPiece = (pieces) => {
      return getRandomPiece(pieces);
    };

    // temporary bot piece selection
    var getRandomPiece = (pieces) => {
      return pieces[Math.floor(Math.random() * pieces.length)];
    }

    // temporary bot piece placement
    var setRandomPiece = (board, piece) => {

    };

    // run game
    var runGame = (board, pieces, turn) => {
      var winner = isOver(board);
      //
      if(winner.orJust()) {
        console.log(winner.just());
      }
      //
      else {
        // player 1 - user turn
        var play = turn == 'p1' ? () => {
          console.log('test2');
        // player 2 - opponent turn
        } : () => {
          var piece = getPiece(pieces); // get piece from opponent
          UI.updateNext(piece); // update UI
          UI.selectTile(runGame, updateBoard, updatePieces, board, piece, pieces, 'p2');
        }
        play();
      }
    };

    // connect to IRC
    var connectIRC = () => {
      // establish connection
      // determine who goes first (p1 == user)
      return 'p2';
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
      UI.setup(init_pieces, pieces_selectors, tiles_selectors); // setup UI

      var turn = configApp();
      runGame(init_board, init_pieces, turn);
    };

    initApp();
  }
});
