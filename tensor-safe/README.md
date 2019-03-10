# tensor-safe

## Instalation instructions

1. Install `ghc-mod`, `hpack` and `stylish-haskell` with `stack install`
2. Run `stack build` in project folder
3. Install `Intero` https://gitlab.com/vannnns/haskero/blob/master/client/doc/installation.md

## Desirable example

```
Network

[ Conv2D 1 10 5 5 1 1, Pooling 2 2 2 2, Relu
, Conv2D 10 16 5 5 1 1, Pooling 2 2 2 2, Reshape, Relu
, Dense 256 80, Logit, Dense 80 10, Logit]

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

## Current working examples

Checkout the `TensorSafe.Examples` module:

```haskell
type MyNet = Network
             '[
                 MaxPooling 2 2 2 2,
                 Flatten,
                 Dense 196 10,
                 Logit,
                 Relu
              ]
             '[
                 'D2 28 28,
                 'D2 14 14,
                 'D1 196,
                 'D1 10,
                 'D1 10,
                 'D1 10
                --  'D1 11 -- doesn't work!!!
              ]
```

## New desirable example

Since for the way of building Networks with the instanciated shapes is a bit tricky and also
pretty annoying for the developers, it would be nicer to define a Generic Network type which
defines just the Layers concatenation type like:

```haskell
type MyNetwork = Network
               '[
                   MaxPooling 2 2 2 2,
                   Flatten,
                   Dense 196 10,
                   Logit,
                   Relu
                ]
```

With this and an input shape, then we could create a type function capable of returning all the
expected outputs of the network. For instance:

```haskell
type MyNetworkShapeFlow = ShapeFlow MyNetwork ('D2 28 28)
-- :t MyNetworkShapeFlow = '[
--                         'D2 28 28,
--                         'D2 14 14,
--                         'D1 196,
--                         'D1 10,
--                         'D1 10,
--                         'D1 10
--                       ]
```

So that we can create an instance of a Network like so:

```haskell
mkINetwork :: (net :: Netowork layers) -> (in :: Shape) -> INetwork layers (ShapeFlow MyNetwork in)


net  :: MyNetwork
iNet :: INetwork '[ layers ] '[ outputs ]
iNet = mkINetwork n (proxy ('D2 28 28))
```
