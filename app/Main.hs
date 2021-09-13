module Main where

-- Get a random word from a list of words(use an API maybe)
-- A function prompt: begin the prompt by showing the blank spaces --> "Guess a letter: "
    -- If the letter is present in word, add the letter to blank space or take off a life
-- If an already guessed letter is guessed again: Notify the user --> "Letter already guessed"
-- If all letters are guessed correctly: Win else Lose

prompt hangman template = do 
    renderHangman hangman
    renderTemplate template

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
                            prompt newHangman newTemplate
                else
                    prompt newHangman newTemplate


main :: IO ()
main = do
    putStrLn "Hangman Starts!"

    word <- getWord

    hangman <- makeHangman
    template <- makeTemplate word

    prompt hangman template 

    putStrLn "Hangman Ends!"

    

