// jQuery tile selectors
var getTileSelector = (x, y) => {
  return $('tr[y="' + y + '"]').children('td[x="' + x + '"]');
};
var curriedGetTiles = R.curry(getTileSelector);
var tiles_selectors = () => {
  return R.ap(R.map(curriedGetTiles, [0, 1, 2, 3]), [0, 1, 2, 3]);
};

// jQuery pieces selectors
var pieces_selectors = () => {
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

// hide opponent piece selection
var hideNext = () => {
  $('.p1-select').show();
  $('.p2-select').hide();
};

// show opponent piece selection
var showNext = () => {
  $('.p1-select').hide();
  $('.p2-select').show();
};

// update piece to play
var updateNext = (classes) => {
  $('#next-piece').removeClass();
  $('#next-piece').addClass('piece');
  classes.forEach((x) => {
    $('#next-piece').addClass(x);
  });
  showNext();
};

// piece and tile selection -- not a very functional solution
var selected_piece = undefined;
var clearSelectedPiece = () => {
  selected_piece = undefined;
};
var selected_tile = undefined;
var clearSelectedTile = () => {
  selected_tile = undefined;
};

var attachBoardListener = () => {

};

var attachPieceListener = () => {

};

var attachListeners = () => {
  // piece selectors
  pieces_selectors().forEach((selector) => {
    // console.log(selector);
    selector.on('click', () => {
      if(typeof selected_piece !== 'undefined'){
        selected_piece.removeClass('select');
      }
      selector.addClass('select');
      selected_piece = selector;
    });
  });
  // tile selectors
  tiles_selectors().forEach((selector) => {
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
var getTile = () => {
  if(typeof selected_tile !== 'undefined') {
    return {
      x: parseInt(selected_tile.attr('x')),
      y: parseInt(selected_tile.parent().attr('y'))
    };
  }
  else {
    return undefined;
  }
};

// get selected piece
var getPiece = () => {
  if(typeof selected_piece !== 'undefined') {
    return getPieceClasses(selected_piece);
  }
  else {
    return undefined;
  }
};

var selectTile = (game, updateBoard, updatePieces, board, piece, pieces, turn) => {
  if(turn === 'p2') {
    $('.select-button').on('click', () => {
      var new_board = updateBoard(board, piece, getTile().x, getTile().y);
      var new_pieces = updatePieces(piece, pieces);
      playPiece(getTile().x, getTile().y, piece);
      hideNext();
      removePiece(piece);
      clearSelectedTile();
      $('.select-button').unbind(); // reset button binding
      $('.select-button').on('click', () => {
        var opponent_piece = getPiece();
        game(new_board, new_pieces, 'p1');
      });
    });
  }
};

// setup UI
var setup = (pieces) => {
  applyClassesToSelectors(pieces, pieces_selectors());
  attachListeners();
};
