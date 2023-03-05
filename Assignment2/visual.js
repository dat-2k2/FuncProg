const boardview = document.querySelector('.boardview');
let numberSolutions = 5;

for (let sol = 0; sol < numberSolutions; sol++){
    const board = document.createElement('div');
    board.style.display = 'flex';
    board.style.flexWrap = 'wrap';
    board.style.border = '1px solid green';
    board.style.margin = '10px';
    board.style.width = boardview.clientWidth / 4 + 'px';
    board.style.height = boardview.clientHeight / 4 + 'px';
    boardview.append(board);

    for (let rows = 0; rows < 8; rows++){
        let boxColor;
        let boxStartColorWhite = rows % 2 === 0? true: false;
        for (let columns = 0; columns < 8; columns++){
            if(boxStartColorWhite)
                boxColor = columns % 2 === 0? 'lightyellow' : 'brown';
            else
                boxColor = columns % 2 === 0? 'brown' : 'lightyellow';
            const box = document.createElement('div');
            box.style.width = board.clientWidth / 8 + 'px';
            box.style.height = board.clientHeight / 8 + 'px';
            box.style.backgroundColor = boxColor;
            board.append(box);
        }
    }
}

