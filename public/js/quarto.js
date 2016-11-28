'use strict';

var modules = () => {
  return (typeof R !== 'undefined' &&        // ramda js -- functional library
          typeof Maybe !== 'undefined' &&    // monet js -- Maybe monad
          typeof Either !== 'undefined' &&   // monet js -- Either monad
          typeof UI !== 'undefined');        // web app UI with jQuery
};

$(() => {
  if(!modules()) {
    console.log('Modules missing.');
  }
  else {
    // monet js -- monadic library
    R.Maybe = Maybe;
    R.Either = Either;

    // initial empty board where each tile is Nothing
    var empty_board = R.repeat(R.repeat(R.Maybe.Nothing(), 4), 4);

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

    // jQuery pieces selectors
    var pieces_selectors = [$('#p0'), $('#p1'), $('#p2'), $('#p3'),
                            $('#p4'), $('#p5'), $('#p6'), $('#p7'),
                            $('#p8'), $('#p9'), $('#p10'), $('#p11'),
                            $('#p12'), $('#p13'), $('#p14'), $('#p15')];

    // append list of classes to a jQuery selector
    var appendClasses = (classes, selector) => {
      R.forEach((x) => {
        selector.addClass(x);
      }, classes);
      // selector.on('click', () => {
      //   console.log('hello from', selector);
      // });
    };

    // apply list of classes to list of jQuery selectors
    var applyClassesToSelectors = (classes, selectors) => {
      R.zipWith(appendClasses, classes, selectors);
    };

    var getPieceClasses = (piece) => {
      return R.reduce((x, y) => {
        return x + ' ' + y;
      }, '', piece);
    };

    // connect to IRC
    var connectIRC = () => {

    };

    // check board for winner
    var isOver = (board) => {
      // return R.Maybe.Just('p1');
      return R.Maybe.Nothing();
    };

    // select piece for opponent
    var selectPiece = () => {

    };

    // run game
    var runGame = (board, pieces) => {
      // test
      UI.playPiece(0, 0, getPieceClasses(pieces[1]));

      var winner = isOver(board);
      //
      if(winner.orJust()) {
        console.log(winner.just());
      }
      //
      else {
        console.log('test');
      }
    };

    // initialize web app
    var initApp = () => {
      applyClassesToSelectors(init_pieces, pieces_selectors); // generate UI pieces
      UI.eventListeners(pieces_selectors); // attach listeners
      runGame(empty_board, init_pieces);
    };

    initApp();
  }
});
