module Main where

import Data.List.Zipper
import qualified Data.Maybe as M

-- Get a random word from a list of words(use an API maybe)
-- A function prompt: begin the prompt by showing the blank spaces --> "Guess a letter: "
    -- If the letter is present in word, add the letter to blank space or take off a life
-- If an already guessed letter is guessed again: Notify the user --> "Letter already guessed"
-- If all letters are guessed correctly: Win else Lose

getWord :: IO String
getWord = return "hangman" -- http://random-word-api.herokuapp.com/word

makeTemplate :: String -> IO (Zipper Char)
makeTemplate = return . fromList

-- template = [h,a,n,g,m,a,n]

makeHangman :: IO (Zipper Bool)
makeHangman = return $ fromList (replicate 6 False)

-- hangman = [False, False, False, False, False, False]

renderHangman :: Zipper Bool -> IO ()
renderHangman _ = return ()

renderTemplate :: Zipper Char -> String -> IO ()
renderTemplate template word = do
    let
        finalData = foldl (\ mydata chr -> 
                let
                    t1 = M.fromMaybe '$' (safeCursor $ zippy mydata)
                    tp = toPrint mydata
                    toPrintNew = if t1 == chr
                        then tp ++ "-"
                        else tp ++ [chr]
                    template2 = if t1 == chr
                        then right $ zippy mydata
                        else zippy mydata   
                in Mydata {zippy=template2, toPrint=toPrintNew})

            (Mydata {zippy=template, toPrint=""})
            word
    putStrLn $ toPrint finalData

data Mydata = Mydata {
    zippy :: Zipper Char,
    toPrint :: String
} deriving (Show)

updateTemplate :: Zipper Char -> String -> IO (Zipper Char)
updateTemplate oldTemp guess = do
    let
        userWord = head guess
        newTemp = foldlz 
            (\newT oldT -> 
                if cursor oldT == userWord
                    then
                        newT
                    else
                        push (cursor oldT) newT

            ) (empty :: Zipper Char) oldTemp
    return $ start newTemp

updateHangman :: Zipper Bool -> IO (Zipper Bool)
updateHangman = return . right . replace True

isTemplateComplete :: Zipper Char -> Bool
isTemplateComplete template =
    let
        lst = toList template
    in
        null lst

isHangmanComplete :: Zipper Bool -> Bool
isHangmanComplete hangman = all id (toList hangman)

prompt word hangman template = do
    renderHangman hangman
    renderTemplate template word

    userGuess <- getLine
    -- TODO: validate user input 

    -- Update template
    newTemplate <- updateTemplate template userGuess

    if isTemplateComplete newTemplate
        then
            putStrLn "You are smart, you win!"
        else
            if newTemplate == template
                then do

                    -- Update hangman
                    newHangman <- updateHangman hangman
                    putStrLn "Wrong Guess!"

                    -- Check if hangman is hanged :)
                    if isHangmanComplete newHangman
                        then
                            putStrLn "You lose!"
                        else
                            prompt word newHangman newTemplate
                else
                    prompt word hangman newTemplate


main :: IO ()
main = do
    putStrLn "Hangman Starts!"

    word <- getWord

    hangman <- makeHangman
    template <- makeTemplate word

    prompt word hangman template

    putStrLn "Hangman Ends!"