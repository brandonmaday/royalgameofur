#! /bin/zsh

cd client
if elm make src/Main.elm --output=elm.bin.js;
then
    mv elm.bin.js ../static
fi
