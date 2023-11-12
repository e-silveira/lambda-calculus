cd src
happy Parser.y
ghc Main.hs -no-keep-hi-files -no-keep-o-files
mv Main ..
cd ..
