# tensor-safe

## Instalation instructions

1. Install `ghc-mod`, `hpack` and `stylish-haskell` with `stack install`
2. Run `stack build` in project folder
3. Install `Intero` https://gitlab.com/vannnns/haskero/blob/master/client/doc/installation.md

## Desirable example

```
Network

[ Conv2D 1 10 5 5 1 1, Pooling 2 2 2 2, Relu
, Convolution 10 16 5 5 1 1, Pooling 2 2 2 2, Reshape, Relu
, FullyConnected 256 80, Logit, FullyConnected 80 10, Logit]

[ 'Shape [28 28], 'Shape [24 24 10], 'Shape [12 12 10], 'Shape [12 12 10]
, 'Shape [8 8 16], 'Shape [4 4 16], 'Shape [256], 'Shape [256]
, 'Shape [80], 'Shape [80], 'Shape [10], 'Shape [10]]
```

```
Conv2D :: Nat -- number of channels (depth)
       -> Nat -- number of filters
       -> Nat -- number of rows in kernel filter
       -> Nat -- number of cols in kernel filter
       -> Nat -- the row stride in the conv filter
       -> Nat -- the col stride in the conv filter
       -> *
```
