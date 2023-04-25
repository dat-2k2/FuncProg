import Data.Char (intToDigit, isDigit)

--newtype Game [moves] moves  = Game {runGame:: log -> (moves, log)}

cheater ::Int->Int
cheater _ = 0

simpletonCoop::Int->Int
simpletonCoop a = a

simpletonCheat::Int->Int
simpletonCheat a = 1-a

grudgerCoop:: Int->Int
grudgerCoop _ = 1

grudgerCheat:: Int->Int
grudgerCheat _ = 0


-- once I turned into bad, there is no way to return !!
data Game  = Game{
    rounds::Int, 
    mode:: (Bool, Bool),
    logGame:: [(Int,Int)],
    points::(Int,Int)
}deriving (Eq, Show)

addMoves :: Game-> (Int,Int)-> Game
addMoves current a = Game (rounds current) newMode (logGame current ++ [a]) (npoint1, npoint2)
    where 
        newMode = (snd a /= 0 && fst (mode current), fst a /= 0 && snd (mode current))
        npoint1 = fst (points current) + (if snd a /= 0 then 3 else 0) - (if fst a /= 0 then 1 else 0)
        npoint2 = snd (points current) + (if fst a /= 0 then 3 else 0) - (if snd a /= 0 then 1 else 0)


partoString :: (Int, Int) -> String
partoString a = "("++ show (fst a) ++ ","++show (snd a) ++")"

serializeData:: Player-> Player->Game -> String
serializeData player1 player2 game =
    "{player_id:[" ++ show (typePlayer player1) ++","++ show (typePlayer player2)++"],log:[" ++ init (concat(map (\x -> partoString x ++ ",") (logGame game))) ++ "],points:"++partoString (points game)++"}"

data Player =  Player {
    typePlayer:: Int,
    coopMove:: Int->Int, 
    cheatMove:: Int->Int
}
-- Coop | Cheat

choosePlayer:: Char -> Player
choosePlayer a 
    | a == '0' = Player 0 cheater cheater
    | a == '1' = Player 1 simpletonCoop simpletonCheat
    | a == '2' = Player 2 grudgerCoop grudgerCheat  

makeMove:: Player->Bool->Int->Int
makeMove player mode prev = 
    if mode then coopMove player prev else cheatMove player prev

-- simpleton: try start cooperate. if you cooperate back, i do same thing as last move, even if it mistake. if you cheat back, i do opposite thing as last move, even if it mistake.
-- grudger: coop until you cheat, otherwise cheat
-- implement player as a State instance
-- make a random choice for cheat or not 

runGame :: Game -> Player -> Player -> Game
runGame current player1 player2 = 
    if length (logGame current) >= rounds current then current else
        if length (logGame current) == 0 then
            runGame (addMoves current (coopMove player1 1, coopMove player2 1)) player1 player2
        else
            runGame (addMoves current (makeMove player1 mode1 $ fst $ last $ logGame current, makeMove player2 mode2 $ snd $ last $ logGame current)) player1 player2
            where 
                mode1 = fst $ mode current
                mode2 = snd $ mode current


runGameIO:: Game -> Player->Int-> IO Game
runGameIO currentGame bot currentRound = 
    if currentRound > 0 then do
        print "Log:"
        print (show $ logGame currentGame)
        print "Your turn, 0 to coop, otherwise cheat: "
        userMoveInput <- getLine -- IO String getLine, then bind result to userMove
        let nextGame = addMoves currentGame (read userMoveInput::Int, makeMove bot (snd $ mode currentGame) (if not (null (logGame currentGame)) then snd $ last $ logGame currentGame else coopMove bot 1))
        runGameIO nextGame bot (currentRound-1)
        else 
            return currentGame
--- Be suggested to use Control.Monad.when but no thanks ima stay basic 

            

    
main::IO()
main = do
    print "Choose mode: bot vs bot (0) / player vs bot (1)"
    isBotBot <- getLine
    if (read isBotBot::Int) == 0 then do
        print "Choose round"
        rounds <- getLine
        print "Choose player 1 : Cheater (0) Simpleton (1) Grudger(2)"
        pick1 <- getLine
        print "Choose player 2 : Cheater (0) Simpleton (1) Grudger(2)"
        pick2 <- getLine
        let player1id = head pick1
            player2id = head pick2
        if (player1id == '0' || player1id == '1' || player1id == '2') && (player2id == '0' || player2id == '1' || player2id == '2') && isDigit (head rounds) then do
            let roundcount = read rounds::Int
            let player1 = choosePlayer player1id
            let player2 = choosePlayer player2id
            let game = runGame (Game roundcount (True,True) [] (0,0)) player1 player2
            let path = "result.txt"
            print game
            writeFile path (serializeData player1 player2 game)
        else 
            print "error"
    else do
        print "Choose round"
        rounds <- getLine
        print "Choose bot : Cheater (0) Simpleton (1) Grudger(2)"
        pick <- getLine
        let botId = head pick
        if (botId == '0' || botId == '1' || botId == '2') && isDigit (head rounds) then do
            let roundcount = read rounds::Int
            let bot = choosePlayer botId
            result <- runGameIO (Game roundcount (True,True) [] (0,0)) bot roundcount
            let path = "result.txt"
            print ("Result: Your Point: " ++ show (fst $ points result) ++ "Opponent's Point:" ++ show (snd $ points result))
            writeFile path (serializeData (Player 4 (const 0) (const 0) ) bot result)
        else 
            print "error"