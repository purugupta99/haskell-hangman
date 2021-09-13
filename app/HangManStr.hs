module HangManStr where

hang0 = "\n\
\  +---+\n\
\  |   |\n\
\      |\n\
\      |\n\
\      |\n\
\      |\n\
\========="

hang1 = "\n\
\  +---+\n\
\  |   |\n\
\  O   |\n\
\      |\n\
\      |\n\
\      |\n\
\========="

hang2 = "\n\
\  +---+\n\
\  |   |\n\
\  O   |\n\
\  |   |\n\
\      |\n\
\      |\n\
\========="

hang3 = "\n\
\  +---+\n\
\  |   |\n\
\  O   |\n\
\ /|   |\n\
\      |\n\
\      |\n\
\========="

hang4 = "\n\
\  +---+\n\
\  |   |\n\
\  O   |\n\
\ /|\\  |\n\
\      |\n\
\      |\n\
\========="

hang5 = "\n\
\  +---+\n\
\  |   |\n\
\  O   |\n\
\ /|\\  |\n\
\ /    |\n\
\      |\n\
\========="

hang6 = "\n\
\  +---+\n\
\  |   |\n\
\  O   |\n\
\ /|\\  |\n\
\ / \\  |\n\
\      |\n\
\========="

getHangMan n = case n of
    0 -> hang0
    1 -> hang1
    2 -> hang2
    3 -> hang3
    4 -> hang4
    5 -> hang5
    6 -> hang6
    _ -> "Not possible"
    