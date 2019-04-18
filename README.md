# Tensor Safe

`tensor-safe` is a framework to define deep learning models which structure is verified on
compilation time. If the models are valid, these can be compiled to Keras framework in Python
or JavaScript.

## Instalation instructions

1. Install `ghc-mod`, `hpack` and `stylish-haskell` with `stack install`

   ```
   cd ~
   stack install ghc-mod hpack stylish-haskell
   ```

2. Run `stack build` in project folder
3. Install `Intero`

   Run `stack build intero` in the project folder

   Ref: https://gitlab.com/vannnns/haskero/blob/master/client/doc/installation.md

## Generate `.cabal` file

Run `hpack` in the root of the project and the file `tensor-safe.cabal` will be generated

## Model definition

Models can be defined as a type using the `MkINetwork` type function. The `MkINetwork` defines a
valid instance of a Network model given a list of `Layers` and a spected input and iutput `Shapes`.

Here's an example of how to define a simple model for the `MNIST` dataset, using `Dense` layers:

```haskell
type MNIST = MkINetwork
    '[
        Flatten,
        Dense 784 42,
        Relu,
        Dense 42 10,
        Sigmoid
    ]
    ('D3 28 28 1)    -- Input
    ('D1 10)         -- Output
```

After that, variable with the model type can be verified with the function `mkINetwork` like this:

```haskell
mnist :: MNIST
mnist = mkINetwork
```

## Nesting networks definitions

You can nest networks definitions easily by adding the networks as layers. For example, in the case of the `MNIST` model defined above, we can abstract the use of Dense and a activation function like this:

```haskell
type DenseRelu i o =
    MkINetwork '[ Dense i o, Relu ] ('D1 i) ('D1 o)

type DenseSigmoid i o =
    MkINetwork '[ Dense i o, Sigmoid ] ('D1 i) ('D1 o)

type MNIST = MkINetwork
    '[
        Flatten,
        DenseRelu 784 42,
        DenseSigmoid 42 10
    ]
    ('D3 28 28 1)    -- Input
    ('D1 10)         -- Output
```

## Command line interface

> This interface will change in the near future

You can install `tensor-safe` command line tool by running `stack build`. Then you can use it by using `stack exec tensor-safe -- check --path ./path-to-model.hs` or `stack exec tensor-safe -- compile --path ./path-to-model.hs --module-name SomeModule`.

## Tools for JavaScript environment

Add as development dependency the packages `babel-plugin-tensor-safe` and `eslint-plugin-tensor-safe`. These can be found in the `extra/javascript` folder in this project.

You can add them directly from this project like this:

```bash
yarn add --dev file/:<path-to-tensor-safe>/extra/javascript/babel-plugin-tensor-safe

yarn add --dev file/:<path-to-tensor-safe>/extra/javascript/eslint-plugin-tensor-safe
```

Then add to the `.eslintrc.js` file in your JavaScript project the plugin `tensor-safe` and the rule `tensor-safe-model-invalid` like this:

```js
module.exports = {
  plugins: [
     ...
     "tensor-safe"
   ],
  ...
  rules: {
    ...
    "tensor-safe/invalid-model": 1
    ...
  }
};
```

And for the Babel plugin add `"@babel/plugin-tensor-safe"` to the plugins list in the `.babelrc` file inside your JavaScript project.

Then, you can write your deep learning model inside your JS files as in the following example:

```js
function createConvModel() {
  safeModel`
    '[
        Conv2D 1 16 3 3 1 1,
        Relu,
        MaxPooling 2 2 2 2,
        Conv2D 16 32 3 3 1 1,
        Relu,
        MaxPooling 2 2 2 2,
        Conv2D 32 32 3 3 1 1,
        Relu,
        Flatten,
        Dense 288 64,
        Sigmoid,
        Dense 64 10,
        Sigmoid
    ]
    ('D3 28 28 1)  -- Input
    ('D1 10)       -- Output
`;

  return model;
}
```

## Related projects

This project was highly influenciated by [Grenade](https://github.com/HuwCampbell/grenade) 💣.
Grenade is a really cool library to define deep neural networks which are validated using dependent types.
What differences TensorSafe from Grenade the most is that TensorSafe doesn't run nor train the models, instead
it compiles the model to external languages that are capable of performing all computations – like Keras
for Python or JavaScript. Also, TensorSafe doesn't need to specifically declare all Shapes transformations
for all the model layers, instead, it just needs the `input` and `output` Shapes to validate the model.

Another worth looking library is [TensorFlow for Haskell](https://github.com/tensorflow/haskell).
This library has all bindings for TensorFlow in C. The issue with this is that it doesn't perform
a lot of type checkings at compilation time. However, there's an open branch that uses dependent
types to solve many of these issues: https://github.com/helq/tensorflow-haskell-deptyped, but the
solution still seems rather complicated for real use.
