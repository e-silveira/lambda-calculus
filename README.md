# Cálculo Lambda

Para rodar o REPL, execute:
```sh
./build.sh
./Main
```

Ou compile direto:
```sh
cd src
ghc Main.hs
```

Exemplos:
```
./Main
> lambda x . x
(lambda x . x)
> (lambda x . x) (lambda y . y)
(lambda y . y)
```