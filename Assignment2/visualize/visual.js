const boardview = document.querySelector('.grid');
link = "test.json"
let Sols = [];
let numberSolutions = 0;
const maxitem = 100;
let size;
const pagelist = [];
const diffver = false;
let numpage = 0;
let hiddenboardindex=[];


fetch("/test.json") // fetch the file 'test.json'
 .then(response => response.json()) // parse the response into JSON
 .then(jsonString => { // parse the JSON string into an array
   for (let i =0; i < jsonString.length; i++){
        Sols.push(jsonString[i].path);
   }
    numberSolutions = Sols.length;
    size = Sols[0].length; 
    //make pagelist index array
    while (numpage < numberSolutions){
        pagelist.push([numpage, numpage + maxitem - 1]);
        numpage = numpage + maxitem;
    }
    pagelist[pagelist.length - 1][1] = numberSolutions - 1;
    
    //store the current numpage
    numpage = 0;
    document.querySelector('.title').innerHTML = "All solutions for "+((diffver)? "difficult":"")+" version of "+size+" rooks. Number solutions: "+numberSolutions;

    //updatebutton
    preload();
    update_button();
    load(pagelist[0][0],pagelist[0][1]);
 })
 .catch(err => { // handle any errors
   console.error(err);
 });



function preload(){
    for (let sol = 0; sol < maxitem; sol++){
        board = document.createElement('div');
        board.className = 'board';
        board.style.setProperty('--size', size);
        boardview.append(board);
        for (let rownum = 0; rownum < size; rownum++){
            for (let column = 0; column < size; column++){
                let cell = document.createElement('div');
                cell.style.width = board.clientWidth / size + 'px';
                cell.style.height = board.clientHeight / size + 'px';
                board.append(cell);
                if ((rownum % 2) == (column % 2)) {
                        cell.className = 'odd';
                    }
                else{
                        cell.className = 'even';
                    }
            }
        }
    }
}


function load(start, end){
    let current = start;
    let board = 0;
        for (; board < end-start+1; board++){
            for (let col = 0; col < size; col++){
                    document.getElementsByClassName('board')[board].children[col + (Sols[current][col] - 1)*size].className = 'rook';
                }
            current++;
        }

    hiddenboardindex = [];
    //hide the excessive 
    if (board < maxitem){
        hiddenboard(board, maxitem - 1);
        hiddenboardindex = [board, maxitem-1];
    }
}


function hiddenboard(start, end){
    for (let index = start ;index <= end; index++){
        let tmp = document.getElementsByClassName('board')[index];
        tmp.style.display = 'none';
        for (let cell = 0; cell < tmp.childElementCount; cell++){
            tmp.children[cell].style.display = 'none';
        }
    }
}

function unhiddenboard(start, end){
    for (let index = start ;index <= end; index++){
        let tmp = document.getElementsByClassName('board')[index];
        tmp.style.display = 'grid';
        for (let cell = 0; cell < tmp.childElementCount; cell++){
            tmp.children[cell].style.display = '';
        }
    }
}
function removerook(){
    for (let board = 0; board < document.getElementsByClassName('board').length; board++){
        let tmp = document.getElementsByClassName('board')[board];
        for (let row = 0; row < size; row++){
            for (let col=0; col < size;col++){
                tmp.children[row*size+col].className = (row % 2 == col%2)?'odd':'even';
            }
        }
    }
}
function nextpage(){
    if (numpage <= pagelist.length - 2) 
    {
        if (hiddenboardindex.length){
            unhiddenboard(hiddenboardindex[0],hiddenboardindex[1]);
        }
        removerook();
        load(pagelist[++numpage][0], pagelist[numpage][1]);
    }
    update_button();
}

function previouspage(){
    if (numpage != 0)
    {
        if (hiddenboardindex.length){
            unhiddenboard(hiddenboardindex[0],hiddenboardindex[1]);
        }
        removerook();
        load(pagelist[--numpage][0], pagelist[numpage][1]);
    }
    update_button();
}

function update_button(){
    if (numpage < pagelist.length-1){
        document.querySelector('#next').style.setProperty ("--color", "green");
    }
    else
    document.querySelector('#next').style.setProperty("--color", "gray");

    if (numpage > 0){
        document.querySelector('#prev').style.setProperty("--color", "green");
    }
    else
    document.querySelector('#prev').style.setProperty("--color", "gray");

}
// CSS loading is also slow