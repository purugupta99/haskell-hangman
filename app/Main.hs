module Main where

import              Data.List.Zipper
import qualified    Data.Maybe as M
import              HangManStr (getHangMan)
import              Data.Maybe
import              Network.HTTP.Client
import qualified    Data.Aeson              as J
import              Text.RawString.QQ

-- Get a random word from a list of words(use an API maybe)
-- A function prompt: begin the prompt by showing the blank spaces --> "Guess a letter: "
    -- If the letter is pre>sent in word, add the letter to blank space or take off a life
-- If an already guessed letter is guessed again: Notify the user --> "Letter already guessed"
-- If all letters are guessed correctly: Win else Lose

type Hangman = Int
type Template = Zipper Char

getWord :: IO String
getWord = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest "http://random-word-api.herokuapp.com/word"
    response <- httpLbs request manager
    return $ head . fromJust . J.decode $ responseBody response

makeHangman :: IO Int
makeHangman = return 0

makeTemplate :: String -> IO Template
makeTemplate = return . fromList

renderHangman :: Hangman -> IO ()
renderHangman hangman = putStrLn (getHangMan hangman)

renderTemplate :: Template -> String -> IO ()
renderTemplate template word = do
    let
        tmpLst = toList template
        zippedTups = zip tmpLst word
        finalStr = fmap (\(a, b) -> if a == '*' then b else '-') zippedTups
    putStrLn finalStr

updateTemplate :: Template -> Char -> IO Template
updateTemplate template userGuess = do
    let
        newTemplate = foldlz (\new temp ->
            let
                currLetter = cursor temp
            in
                if currLetter == userGuess
                    then
                        push '*' new
                    else
                        push currLetter new
                    ) empty template
    return $ start newTemplate

updateHangman :: Hangman -> IO Hangman
updateHangman = return . succ

isTemplateComplete :: Template -> Bool
isTemplateComplete template = all (== '*') (toList template)

isHangmanComplete :: Hangman -> Bool
isHangmanComplete hangman = 6 == hangman

prompt word hangman template = do
    renderHangman hangman
    renderTemplate template word

    userGuess' <- getLine
    let userGuess = head userGuess'
    -- TODO: validate user input 

    -- Update template
    newTemplate <- updateTemplate template userGuess

    if isTemplateComplete newTemplate
        then do
            putStrLn $ "The word was: " ++ word
            putStrLn "You are smart, you win!"
        else
            if newTemplate == template
                then do

                    -- Update hangman
                    newHangman <- updateHangman hangman
                    putStrLn "Wrong Guess!"

                    -- Check if hangman is hanged :)
                    if isHangmanComplete newHangman
                        then do
                            putStrLn (getHangMan 6)
                            putStrLn "You lose!"
                            putStrLn $ "The word was: " ++ word
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