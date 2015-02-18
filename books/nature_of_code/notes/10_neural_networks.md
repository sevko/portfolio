# chapter 10: neural networks
A *neural network* is a complex adaptive system.

Three types of machine learning:

  * **Supervised learning**: making a guess, being told the right answer, and learning from it.
  * **Unsupervised learning**: independently making assumptions about data and identifying patterns.
  * **Reinforcement learning**: learning based on observation and stimuli.

The *Perceptron*, invented in 1957 by Frank Rosenblatt, is the simplest neural network possible, consisting of only 1
unit. It follows the feed-forward model: inputs are processed and turned into outputs.

Operates in the following steps:

  1. receive inputs
  2. weigh inputs
  3. sum inputs
  4. generate output

Supervised learning:

  1. make guess
  2. check against expected
  3. adjust weights

```
error = desired - guess
new weight = weight + error * input * learning constant
```

Perceptrons can only solve *linearly separable* problems. In informal terms, the data has to be separable by a straight
line. A *network* of learners, on the other hand, can solve more complex ones.
