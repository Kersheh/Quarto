// jQuery tile selectors
var getTileSelector = (x, y) => {
  return $('tr[y="' + y + '"]').children('td[x="' + x + '"]');
};
var curriedGetTiles = R.curry(getTileSelector);
var tileSelectors = () => {
  return R.ap(R.map(curriedGetTiles, [0, 1, 2, 3]), [0, 1, 2, 3]);
};

// jQuery pieces selectors
var pieceSelectors = () => {
  return [$('#p0'), $('#p1'), $('#p2'), $('#p3'),
          $('#p4'), $('#p5'), $('#p6'), $('#p7'),
          $('#p8'), $('#p9'), $('#p10'), $('#p11'),
          $('#p12'), $('#p13'), $('#p14'), $('#p15')];
};

// append list of classes to a jQuery selector
var appendClasses = (classes, selector) => {
  classes.forEach((x) => {
    selector.addClass(x);
  });
};

// apply list of classes to list of jQuery selectors
var applyClassesToSelectors = (classes, selectors) => {
  R.zipWith(appendClasses, classes, selectors);
};

// get piece classses and strip away 'piece' and 'select' class
var getPieceClasses = (selector) => {
  var classes = selector.attr('class').split(' ');
  return R.reject((x) => { return x == 'piece' || x == 'select'; }, classes);
};

// remove piece from selectable pieces
var removePiece = (classes) => {
  var c = R.flatten(R.map((x) => {
    return R.prepend('.', x);
  }, classes)).join('');
  $('.piece' + c).remove();
};

// play piece at location
var playPiece = (x, y, classes) => {
  var selector = $('tr[y="' + y + '"]').children('td[x="' + x + '"]');
  selector.append('<div class=""></div>');
  selector.removeClass('empty');
  appendClasses(classes, selector.children());
};

// show opponent piece selection
var showNext = () => {
  $('.p1-selected').hide();
  $('.p2-selected').show();
};

// hide opponent piece selection
var hideNext = () => {
  $('.p2-selected').hide();
  $('.p1-select').show();
};

// update selected piece info
var waitNext = () => {
  $('.p1-select').hide();
  $('.p1-selected').show();
};

// update next piece to be played
var updateNext = (classes) => {
  var selector = $('.next-piece');
  selector.removeClass();
  selector.addClass('next-piece');
  classes.forEach((x) => {
    selector.addClass(x);
  });
};

// display winner
var displayWinner = (result) => {
  $('#info-next-piece').hide();
  $('#game-results').show();
  if(result === 'p1') {
    $('#game-results').append(' You have won! :)');
  }
  if(result === 'p2') {
    $('#game-results').append(' Your opponent has won. :(');
  }
  if(result == 'tie') {
    $('#game-results').append(' It\'s a tie!');
  }
};

// piece and tile selection -- not a functional solution using mutable data
var selected_piece = undefined;
var selected_tile = undefined;
var clearSelected = () => {
  selected_piece = undefined;
  selected_tile = undefined;
  $('.select').removeClass('select');
};

var attachListeners = () => {
  // piece selectors
  pieceSelectors().forEach((selector) => {
    selector.on('click', () => {
      if(typeof selected_piece !== 'undefined'){
        selected_piece.removeClass('select');
      }
      selector.addClass('select');
      selected_piece = selector;
    });
  });
  // tile selectors
  tileSelectors().forEach((selector) => {
    selector.on('click', () => {
      if(typeof selected_tile !== 'undefined'){
        selected_tile.removeClass('select');
      }
      selector.addClass('select');
      selected_tile = selector;
    });
  });
};

// get selected tile coordinates
var getTile = (callback, fallback) => {
  if(typeof selected_tile !== 'undefined') {
    selected_tile.unbind(); // remove selection functionality of tile
    x = parseInt(selected_tile.attr('x'));
    y = parseInt(selected_tile.parent().attr('y'));
    callback(x, y);
  }
  // recursive call if no tile selected
  else {
    fallback();
  }
};

// get selected piece
var getPiece = (callback, fallback) => {
  if(typeof selected_piece !== 'undefined') {
    var piece = getPieceClasses(selected_piece);
    callback(piece);
  }
  // recursive call if no piece selected
  else {
    fallback();
  }
};

// set button
var setButton = (callback) => {
  return $('.select-button').on('click', () => {
    $('.select-button').unbind(); // reset button binding after use
    callback();
    clearSelected(); // clear selection
  });
};

var selectTile = (board, piece, pieces, turn) => {
  // player 1 turn
  if(turn === 'p1') {
    // set select button to place piece
    var p1_select_tile = () => {
      setButton(() => {
        getTile((x, y) => {
          playPiece(x, y, piece); // update ui
          var new_board = updateBoard(board, piece, x, y);
          var new_pieces = updatePieces(piece, pieces);
          // update ui
          hideNext();
          removePiece(piece);
          if(!isBoardFull(new_board)) {
            p1_select_piece(new_board, new_pieces);
          }

          else {
            runGame(new_board, new_pieces, piece, 'p2');
          }
        }, () => {
          p1_select_tile();
        });
      });
    };

    // set select button to select piece for opponent
    var p1_select_piece = (board, pieces) => {
      setButton(() => {
        getPiece((piece) => {
          updateNext(piece);
          removePiece(piece);
          waitNext();
          runGame(board, pieces, piece, 'p2');
        }, () => {
          p1_select_piece(board, pieces);
        });
      });
    };

    // execute turn
    if(!isBoardEmpty(board)) {
      updateNext(piece);
      showNext();
      p1_select_tile();
    }
    else {
      hideNext();
      p1_select_piece(board, pieces);
    }
  }
  else {
    // display piece to be played
    updateNext(piece);
    showNext();
    runGame(board, pieces, piece, 'p1');
  }
};

// setup UI
var setup = (pieces) => {
  applyClassesToSelectors(pieces, pieceSelectors());
  attachListeners();
};
