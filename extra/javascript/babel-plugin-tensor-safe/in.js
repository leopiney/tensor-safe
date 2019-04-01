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
      ('D3 28 28 1)    -- Input
      ('D1 10)       -- Output
  `;

  return model;
}
