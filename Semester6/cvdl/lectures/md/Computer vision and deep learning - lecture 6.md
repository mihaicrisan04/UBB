# Computer vision and Deep Learning

Lecture 6: Sequences. Recurrent neural networks. Transformers (I).

BORZA DIANA LAURA

# Today’s agenda

- Sequence models
- Recurrent neural networks
- Transformers

# Sequence models

# Sequences

- Numerical: time series (1d), nd 

- Categorical: characters, words, music keys etc.
- T,h,e,r,e, ,w,a,s, ,a
- There, was, a,
-  
- 

<!-- image -->

<!-- image -->

# Sequence models

Sequence models are applied on sequences of different lengths using the same (shared weights)

- Input sequence
- Output: label (classification), number (regression), token, sequence

- 
- 
- 

# Sequence to sequence layers

- Input: sequence of vectors
- Output: sequence of vectors
- 

Sequence to sequence layers can be applied on input sequences of different lengths (but using the same weights).

Sequence to sequence layers:

- Recurrent neural networks
- Convolutions
- Self-attention layers

# Processing discrete inputs

- One hot encoding
- 
- 

<!-- image -->

# Processing discrete inputs

- Combined in some learning tasks and learned by back-propagation
- Embedding: given a set of discrete values xi  ∈ X, model them by an embedding vector emb[xi], such that if xi  and xj are similar, so are emb[xi] and emb[xj]

<!-- image -->

# Batching sequences

- Padding

<!-- image -->

# Batching sequences: padding

<!-- image -->

# Batching sequences: packing

- You can use lengthwise sorting and use a dynamic batch size
- 

<!-- image -->

# Definitions

- Casuality: the way the elements in a layer are wired up

Casual layer: element xi has access only to elements x0, x1, …, xi-1

# Definitions

- Autoregressive model
- Start with a seed (a few samples): [t0, t1]
- Sample the next token t = 2 from P(t | t0, t1) and append it to the sequence
- Feed the sequence to the network
- REPEAT
- 
- 

<!-- image -->

<!-- image -->

# Definitions

- Teacher forcing
- Scheduled sampling: “probabilistic schedule”

<!-- image -->

https://medium.com/@vivekvscool/translation-or-answer-tool-seq2seq-with-teacher-forcing-and-attention-mechanism-7cfd9cb03b3a 

# Patterns in sequence models

# Process sequences

<!-- image -->

# Process sequences

<!-- image -->

Image captioning, 

Text generation

# Process sequences

<!-- image -->

Action recognition

Sentiment classification

# Process sequences

<!-- image -->

Video classification at frame level

Text translation

# Recurrent neural networks

# Recurrent neural networks

- A recurrent neural network is a network where we can have a cycle between two nodes

x&lt;t&gt;

RNN cell h&lt;t&gt; 

y&lt;t&gt;

# RNN cell

- Recurrent core cell
- Input: x&lt;t&gt;
- Internal hidden state: h&lt;t&gt; 
- updated each time an input x&lt;t&gt; is fed to the cell
- Output: y&lt;t&gt;
- at some time steps

x&lt;t&gt;

RNN cell h&lt;t&gt; 

y&lt;t&gt;

# RNN cell 

x&lt;t&gt;

RNN cell

y&lt;t&gt;

# RNN cell 

x&lt;t&gt;

RNN cell

y&lt;t&gt;

nonlinearity

# RNN cell 

x&lt;t&gt;

RNN cell

y&lt;t&gt;

# RNN cell 

x&lt;1&gt;

RNN cell

y&lt;1&gt;

h&lt;0&gt; = [0, .., 0]

x&lt;2&gt;

x&lt;3&gt;

h&lt;1&gt;

# RNN cell 

x&lt;2&gt;

RNN cell

y&lt;2&gt;

h&lt;1&gt;

x&lt;1&gt;

x&lt;3&gt;

y&lt;1&gt;

h&lt;2&gt;

x&lt;4&gt;

# RNN cell 

x&lt;3&gt;

RNN cell

y&lt;3&gt;

h&lt;2&gt;

x&lt;1&gt;

x&lt;5&gt;

y&lt;1&gt;

x&lt;2&gt;

y&lt;2&gt;

h&lt;3&gt;

x&lt;4&gt;

# RNN backward pass

<!-- image -->

During backpropagation from the next state to the current state we pass through a matrix multiplication step

# RNN backward pass

<!-- image -->

At each cell we multiply by many factors of  the weight matrix W

&gt; 1: exploding gradients (gradient clipping): scale the value of the gradient if it is too big

&lt; 1: vanishing gradients 

# Long short term memory

https://colah.github.io/posts/2015-08-Understanding-LSTMs/ 

# Long short term memory, 1997

- Alleviate the problems of vanishing and exploding gradient
- Selective forgetting and remembering (gates)
- 

<!-- image -->

<!-- image -->

<!-- image -->

RNN

LSTM

Two hidden states:

c – cell state

h – hidden state

# Long short term memory, 1997

- Alleviate the problems of vanishing and exploding gradient

RNN					 LSTM

Two hidden states:

c – cell state

h – hidden state

<!-- image -->

<!-- image -->

# Long short term memory

- Conveyor belt: previous cell state passes to become the next cell state with only some minor linear interactions

<!-- image -->

<!-- image -->

# Long short term memory

<!-- image -->

<!-- image -->

<!-- image -->

Concatenate previous hidden state cell with the current input and multiply them with a bigger weight matrix to compute four gates

<!-- image -->

# LSTM cell

<!-- image -->

# Long short term memory

<!-- image -->

<!-- image -->

<!-- image -->

forget gate: whether to erase cell (how much we want to forget from the previous cell state?)

input gate: whether to write to cell (how much to input into our cell?)	

g gate: how much to write to cell (how much to write into our cell?)

output gate: how much to reveal cell (how much to reveal from ourselves to the outside?

# Long short-term memory Forget gate: how much we want to forget from the previous cell state?/ whether to erase the cell

<!-- image -->

<!-- image -->

<!-- image -->

Sigmoid: [0, 1]

tanh:  [-1, 1]

Element wise multiplication: forget that element of the cell (0), or remember it (1)

<!-- image -->

# Long short-term memory Forget gate: how much we want to forget from the previous cell state?/ whether to erase the cell

- Intuition: “The cell state might include the gender of the present subject, so that the correct pronouns can be used. When we see a new subject, we want to forget the gender of the old subject.”
- Next, we need decide what new information we’re going to store in the cell state. 
1. a sigmoid layer called the “input gate layer” decides which values we’ll update
2. a tanh layer (g  gate) creates a vector of new candidate values, that could be added to the state

https://colah.github.io/posts/2015-08-Understanding-LSTMs/ 

# Long short-term memory Input gate: how much to input into our cell? / whether to write to cell 

<!-- image -->

<!-- image -->

<!-- image -->

Sigmoid: [0, 1]

tanh:  [-1, 1]

Element wise multiplication: for each element of the cell state, do we want to write to (1) it or not (0)?

<!-- image -->

# Long short-term memory Input gate: how much to input into our cell? / whether to write to cell g gate:  how much to write to cell (how much to write into our cell?)

<!-- image -->

<!-- image -->

<!-- image -->

Sigmoid: [0, 1] 

tanh:  [-1, 1]

<!-- image -->

Element wise multiplication: the candidate value that we might consider writing to the current cell state [-1, 1] (creates a vector of new candidate values, that could be added to the state)

# Long short-term memory Input gate: how much to input into our cell? / whether to write to cell g gate:  how much to write to cell (how much to write into our cell?)

- Intuition: “We’d want to add the gender of the new subject to the cell state to replace the old one we’re forgetting.”
- 
- 
- 
- 
- Actually drop the information about the old subject’s gender and add the new information

https://colah.github.io/posts/2015-08-Understanding-LSTMs/ 

<!-- image -->

# Long short term memory output gate: how much to reveal cell (how much to reveal from ourselves to the outside? 

<!-- image -->

<!-- image -->

<!-- image -->

Sigmoid: [0, 1]

tanh:  [-1, 1]

Use the cell state to compute a hidden state which we will reveal to the outside world

Sigmoid: for each element of out cell state, do we want to reveal it or not?

<!-- image -->

 The output will be based on the cell state, but will be a filtered version. 

# Long short term memory output gate: how much to reveal cell (how much to reveal from ourselves to the outside?

- Intuition: “For the language model example, since it just saw a subject, it might want to output information relevant to a verb, in case that’s what is coming next. For example, it might output whether the subject is singular or plural, so that we know what form a verb should be conjugated into if that’s what follows next”

https://colah.github.io/posts/2015-08-Understanding-LSTMs/ 

# Lots of variations of LSTMs were proposed

- Gated Recurrent Unit (GRU) – simplify the architecture of the LSTM

<!-- image -->

# The Unreasonable Effectiveness of Recurrent Neural Networks

- https://karpathy.github.io/2015/05/21/rnn-effectiveness/ 

Shakespeare

PANDARUS: 

Alas, I think he shall be come approached and the day When little srain would be attain'd into being never fed, And who is but a chain and subjects of his death, I should not sleep. 

Second Senator: 

They are away this miseries, produced upon my soul, Breaking and strongly should be buried, when I perish The earth and thoughts of many states. 

DUKE VINCENTIO: 

Well, your wit is in the care of side and that. Second Lord: They would be ruled after this chamber, and my fair nues begun out of the fact, to be conveyed, Whose noble souls I'll have the heart of the wars. 

Clown: Come, sir, I will make did behold your worship. 

VIOLA: I'll drink it.

# The Unreasonable Effectiveness of Recurrent Neural Networks

- https://karpathy.github.io/2015/05/21/rnn-effectiveness/ 

Wikipedia

Naturalism and decision for the majority of Arab countries' capitalide was grounded by the Irish language by [[John Clair]], [[An Imperial Japanese Revolt]], associated with Guangzham's sovereignty. His generals were the powerful ruler of the Portugal in the [[Protestant Immineners]], which could be said to be directly in Cantonese Communication, which followed a ceremony and set inspired prison, training. The emperor travelled back to [[Antioch, Perth, October 25|21]] to note, the Kingdom of Costa Rica, unsuccessful fashioned the [[Thrales]], [[Cynth's Dajoard]], known in western [[Scotland]], near Italy to the conquest of India with the conflict. Copyright was the succession of independence in the slop of Syrian influence that was a famous German movement based on a more popular servicious, non-doctrinal and sexual power post. Many governments recognize the military housing of the [[Civil Liberalization and Infantry Resolution 265 National Party in Hungary]], that is sympathetic to be to the [[Punjab Resolution]] (PJS)[http://www.humah.yahoo.com/guardian. cfm/7754800786d17551963s89.htm Official economics Adjoint for the Nazism, Montgomery was swear to advance to the resources for those Socialism's rule, was starting to signing a major tripad of aid exile.]]

# The Unreasonable Effectiveness of Recurrent Neural Networks

<!-- image -->

# Attention is all you need

https://arxiv.org/abs/1706.03762 

<!-- image -->

# Self-attention

# Self attention

Self attention

x1

x2

x3

x4

x5

x6

x7

x8

y1

y2

y3

y4

y5

y6

y7

y8

X - (b,t,k)

b – batch size

t – sequence length

k – embedding dim

# Self attention

<!-- image -->

# Self attention – so far

- Has no parameters
- It can look far back into the input sequence (every input has the same distance to every output)
- It operates on sets (no information about the sequential structure of the input)

self\_attention(permute(x)) =permute(self\_attention(x))

- 

# Self attention

<!-- image -->

http://peterbloem.nl/blog/transformers 

# Attention: query, keys and values

“An attention function can be described as mapping a query and a set of key-value pairs to an output, where the query, keys, values, and output are all vectors.”

<!-- image -->

# Key, query and values

To obtain the query, keys and values, there matrices are introduced: Wq, Wk, Wv   (added some learnable parameters)

<!-- image -->

# Key, query and values

<!-- image -->

query

value

key

When computing yi, xi is matched against every other input 

# Key, query and values

<!-- image -->

query

value

key

The vector that the query is matched against

# Key, query and values

<!-- image -->

query

value

key

Used in the weighted sum of the output

# Soft version of a dictionary

my\_dict = {‘key1’: 24, ‘key2’: 1, ‘key3’:  6}

my\_dict[key1] == 24

query

key

value