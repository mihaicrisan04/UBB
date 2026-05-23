# Computer vision and deep learning

Lecture 1

# What is Computer Vision (CV)?

Goal: build computer-based vision systems which perform the same functions as the human visual system

CV builds the theoretical and algorithmic basis by which useful information about the world can be extracted and analysed from an image, a set of images (e.g. stereo vision) or an image sequence

<!-- image -->

# Computer vision – multidisciplinary subject

http://vision.stanford.edu/teaching/cs131\_fall1718/files/cs131-class-notes.pdf 

<!-- image -->

# Computer vision vs. human vision 2012 – “we are really, really far away”

<!-- image -->

Example from Andrej Karpathy’s blog: http://karpathy.github.io/2012/10/22/state-of-computer-vision/ 

# Computer vision vs. human vision 2025

<!-- image -->

Example from Andrej Karpathy’s blog: http://karpathy.github.io/2012/10/22/state-of-computer-vision/ 

User: What’s funny about this picture?

Chat gpt o1: It’s essentially a playful “gotcha” moment. One man is standing on a scale to check his weight, and another man behind him (in on the joke) is sneaking a foot onto the same scale. Because of that extra weight, the person on the scale is unintentionally getting a higher reading—hence all the laughter and amused expressions in the background. 

<!-- image -->

Andrej Karpathy

“We should all be playing games and let AI do all the work, instead AI plays games and we do all the work”

# Course content

Fundamentals: Artificial Neural Networks

Human Visual System. Cell vs. Functions

Neural Networks. Optimization. Loss functions

Architectures: Convolutional neural networks, visual transformers, graph convolutional neural networks

Convolutions. Convolutional layers. Pooling layers

Attention mechanisms

Transformers. Visual transformers. Foundation models

Graph convolutional neural networks

Applications and case studies

Segmentation

Object detection

Instance segmentation

Multimodal learning

Image generation

Video understanding

# Laboratory content

Laboratory 1 (1 week)

- images; numpy; torch;

Laboratory 2 (2 weeks)

- softmax, linear classifier from scratch, loss functions, metrics, wandb;

Laboratory 3 (2 weeks)

- convolutions from scratch; CNNs, transfer learning; visualizations;

Laboratory 4 (2 weeks)

- semantic segmentation; data loaders; augmentations;

Laboratory 5 (2 weeks)

- attentions from scratch; transformers; supervised fine tuning; parameter efficient fine tuning;

Laboratory 6 (1 week)

- messy data; yolov26 vs RF-DETR;

Laboratory 7 (2 week) 

- deployment; quantization; serving.

# Grading

- Written exam 60%
- Laboratory 40% (one or two test during the semester)
- 2 points extra: project proposed by the TA [presented at the end of the semester]
- 

Grade = 0.6*(theoretical exam) + 0.4*(laboratory works)+0.2*[project]

(max grade: 12)

# Bibliography

- Foundations of Computer Vision, Antonio Torralba, Phillip Isola, and William Freeman
- https://visionbook.mit.edu/ 
- Mathematics for Machine Learning, Marc Peter Deisenroth, A. Aldo Faisal Cheng Soon Ong 
- https://mml-book.github.io/book/mml-book.pdf 
- Deep learning. Goodfellow, Ian, Yoshua Bengio, and Aaron Courville. MIT press, 2016.
- http://www.deeplearningbook.org/
- CS231n lecture, Stanford - https://cs231n.github.io/ (lecture notes)

# Bibliography

- deeplearning.ai
- https://www.deeplearning.ai/ (Machine Learning Specialization, Deep Learning Specialization – courses also available on youtube)
- Andrew Karpathy – Neural Networks – Zero to Hero
- https://www.youtube.com/@AndrejKarpathy 
- Full Stack Deep Learning
- https://fullstackdeeplearning.com/ 
- Weights and biases free courses
- https://www.wandb.courses/pages/w-b-courses

# Bibliography

- How to read a research paper (Andrew Ng):
- https://forums.fast.ai/t/how-to-read-research-papers-andrew-ng/66892 
- Technical writing (Zachary Lipton): https://www.approximatelycorrect.com/2018/01/29/heuristics-technical-scientific-writing-machine-learning-perspective/ 

# How to read a research paper

- How to Read Research Papers
- Take multiple passes through the paper
- Worst strategy: reading from the first word until the last word!
- Read the Title/Abstract/Figures
- Especially in deep learning, there are a lot of papers where the entire paper is summarized in one or two figures.
- You can often get a good understanding about what the whole paper is about without reading much of the text.
- Read the Introduction/Conclusions/Figures (again)/Skim Rest
- Part of the process of writing papers is convincing the reviewers that your paper is worthy of acceptance, so you find that the abstract, intro, and conclusion are where the authors summarize their work really carefully. These are therefore the most useful parts to read.
- Neural network architectures are often written up in a table.
- Maybe also skim Related work section, for context or see if there is something you have read before.
- Read the Paper, but skip the maths
- Read the Paper, but skip parts that don’t make sense
- In cutting edge papers we don’t always know what is really important and what isn’t important.
- Some great, highly cited papers have some parts which are groundbreaking and other parts which later turn out to be unimportant, but at the time the paper was written the authors could not know.
- Maybe what was the key part of the algorithm wasn’t what the authors thought.
- 

https://forums.fast.ai/t/how-to-read-research-papers-andrew-ng/66892 

# The visual system

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

Image source: Le miracle de la vision, Christian Schwagerl, Rainer Harf et Henning Engeln 

# Evolution of the eye

<!-- image -->

Cambrian Explosion:  541 million years ago

Image source: https://en.wikipedia.org/wiki/Cambrian\_explosion 

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# Flat eye

- The simplest type of eye; also called an eyespot
- Made of light-sensitive cells on a flat surface
- Functions
- Detects light and darkness
- Sometimes detects basic colors
- Cannot focus images
- Where Is It Found?
- Starfish
- Jellyfish
- Some worms
- 

<!-- image -->

<!-- image -->

# Pit eye

- A flat eye that has curved inward
- Forms a small cup or pit
- Functions
- Detects the direction of light
- More precise than a flat eye
- Still cannot form a clear image
- How Does It Work?
- Light enters the pit
- Only certain cells are activated
- The brain estimates where light comes from
- Where Is It Found?
- Limpets
- Some marine worms
- Primitive mollusks
- 

<!-- image -->

<!-- image -->

# Bubble eye

- A pit eye that developed a transparent covering
- Contains a gel-like structure (primitive lens)
- Functions
- Focus light
- Form a clearer image
- See shapes and movement
- The “bubble” concentrates light
- Produces a brighter and sharper image
- Where Is It Found?
- Some snails
- Marine mollusks
- Early invertebrates
- 

<!-- image -->

<!-- image -->

# Vertebrates eye

- A complex eye with:
- Cornea
- Pupil
- Lens
- Retina
- Functions
- Perceives detailed images
- Sees colors
- Detects depth and movement
- Adjusts to light levels
- How Does It Work?
- Light enters through the cornea
- Lens focuses light on the retina
- Retina sends signals to the brain
- Brain creates the final image
- 

<!-- image -->

# History of the camera

<!-- image -->

18th century description of a pinhole camera

Encyclopedia

- Chinese philosopher Mozi (470 – 391 BC) first person in history to describe the first to describe the physical principle behind the camera
- Alhazen (Ibn al-Haytham) (965–1040 AD) – considered the inventor of the pinhole camera
- First permanent photograph (1825) by Joseph Nicéphore Niépce (8h exposure time)

<!-- image -->

View from the Window at Le Gras, 1827 

Oldest surviving camera photograph

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

1500: Camera obscura is a natural phenomenon that occurs when a device in the shape of a box lets light through a small opening on one side and projects it on to the other. The projected image is both reversed and inverted.

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

1839 – Daguerreotype Camera. Louis Daguerre, developed a process to store the projected image on a mirror finished copper plate treated with chemicals.​ The first time an object was made to be light sensitive to capture information and store it forever.

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

1900 – The Reise Camera - the world’s first portable camera.

​It was extremely light weight and could be folded into a compact shape, meaning people could take photos on the move. 

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

1925 – The Leica 1 was the first commercially available compact 35mm camera. 

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

1947 - Polaroid 95

The Polaroid Model 95 enabled photographers to take a photo and then print it immediately, utilising special paper provided by Polaroid. 

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

1975 –  Steven Sasson developed the Kodak Digital Camera

The first digital camera was made commercially available in 1991 - although it came with a hefty price tag of $13,000!

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

2000 - Sharp created the first mobile phone with a tiny inbuilt digital camera.

<!-- image -->

# Evolution of the camera

Image source: https://www.precisioncamerarepairs.com/blog/the-evolution-of-the-camera 

<!-- image -->

Kyocera Visual Phone VP-210 - 1999

1975

1925 – The Leica I

1839

1947 – Polaroid Model 95

2007 – Apple iPhone 

2008 – GoPro HD HERO

1900 – The Reise Camera 

# Nowadays…

-  ~ 1.436.300.000.000 ( &gt; 1.4 trillion) photos  will be taken in 2020 (https://focus.mylio.com/tech-today/how-many-photos-will-be-taken-in-2020 )
- If you were to take all these pictures alone, and you took one pic per second = 45.544 years 
- As of May 2019, &gt; 500 hours of video are uploaded to YouTube every minute!  (https://www.statista.com/statistics/259477/hours-of-video-uploaded-to-youtube-every-minute/) 
-  an estimate of 770 million surveillance cameras installed around the world today

# Visual cortex

<!-- image -->

https://courses.lumenlearning.com/wmopen-psychology/chapter/outcome-vision/ 

<!-- image -->

Visual information goes from the retina → optic nerve → visual cortex (occipital lobe).

Then it splits into two pathways:

- Ventral pathway: “what” pathway (temporal)

- Dorsal pathway: “where/how” pathway (parietal)

# Visual cortex

<!-- image -->

Ventral Pathway: “What” Pathway 

- Goes to the temporal lobe
- Identifies what we are looking at
- Recognizes: faces, objects, colors, etc.
- 

Example:

You see an object and recognize:

“That is a dog.”

If damaged the person can see the object, but cannot recognize what it is (visual agnosia)

# Visual cortex

<!-- image -->

Dorsal Pathway: “Where/How” Pathway 

- Goes to the parietal lobe
- Detects where an object is
- Processes movement, position, spatial coordination, hand-eye coordination
- 

Example:

You reach and grab a cup without missing it.

If damaged the person can see the object, but it cannot accurately reach for it (optic ataxia)

# Understanding the visual cortex

Nobel Prize for Physiology or Medicine in 1981:

David Hubel and Torsten Wiesel 

<!-- image -->

Image source: https://www.researchgate.net/publication/335707980\_Understanding\_Neural\_Networks\_via\_Feature\_Visualization\_A\_Survey 

Simple cells: 

	orientation, position

Complex cells: 

	orientation, motion, direction

“Hypercomplex” cells:

	orientation, motion, direction, length

Hubel and Wiesel, 1959

https://www.youtube.com/watch?v=IOHayh06LJ4&amp;ab\_channel=PaulLester 

# Understanding the visual cortex

- https://www.youtube.com/watch?v=IOHayh06LJ4&amp;ab\_channel=PaulLester 
- https://youtu.be/dBHFxAWC8I0

# Larry Roberts

1963

First PhD thesis in the field of Computer Vision: “Machine Perception of Three Dimensional Solids”, Larry Roberts,  1963, MIT Press

- extract 3D information about solid objects from 2D photographs of line drawings

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# The Summer Vision Project

1966

<!-- image -->

<!-- image -->

The primary goal of the project is to construct a system of programs which will divide a vidisector picture into regions such as likely objects, likely background areas and chaos. We shall call this part of its operation FIGURE-GROUND analysis. It will be impossible to do this without considerable analysis of shape and surface properties, so FIGURE-GROUND analysis is really inseparable in practice from the second goal which is REGION DESCRIPTION. The final goal is OBJECT IDENTIFICATION which will actually name objects by matching them with a vocabulary of known objects.

# David Maar

1981

<!-- image -->

Image

Primal sketch

Relational Structure

Recognition description

Scene

2.5 Surface Map

2.5D Symbolic map

Pixel intensities

Edges, blobs, curves boundaries

Depth image, surface orientations

Object centred coordinate system (surface and geometrical primitives)

# Canny edge detector

1986

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

Images from https://towardsdatascience.com/canny-edge-detection-step-by-step-in-python-computer-vision-b49c3a2d8123 

# Scale Invariant Feature Transform David G. Lowe

1999

Local image features invariant to image scaling, translation, and rotation, and partially invariant to illumination changes and affine or 3D projection

<!-- image -->

# Rapid object detection using a boosted cascade of simple features Paul Viola and Michael Jones

<!-- image -->

<!-- image -->

2001

<!-- image -->

# Histogram of Oriented Gradients Navneet Dalal and Bill Triggs

2005

<!-- image -->

Image source: https://iq.opengenus.org/object-detection-with-histogram-of-oriented-gradients-hog/ 

<!-- image -->

<!-- image -->

AlexNet (2012)

<!-- image -->

LeNet (1998)

2012

# Visual transformers

2021

https://arxiv.org/abs/2010.11929 

<!-- image -->

# Vision language models (VML)

2023

<!-- image -->

# Vision language models (VML)

2023

<!-- image -->

# Data

<!-- image -->

# Pascal VOC

2007

- 9,963 images containing  24,640 object split into 20 classes: 
- airplane, bicycle, boat, bottle, bus, car, cat, chair, cow, dining table, dog, horse, motorbike, person, potted plant, sheep, train, TV

<!-- image -->

# ImageNet

2010

<!-- image -->

- &gt; 14 million images organized into over  20,000 categories  
- organized according to the WordNet hierarchy (nouns), in which each node of the hierarchy is depicted by hundreds and thousands of images

<!-- image -->

# JFT

2017

- internal dataset used by Google to train image classification algorithms
- the dataset has 300M images and 375M labels (multiple labels per image, on average each image has 1.26 labels)
- noisy labels: no human processing is involved
- labeled using an algorithm that uses complex mixture of raw web signals, connections between webpages and user feedback

<!-- image -->

<!-- image -->

https://arxiv.org/pdf/1707.02968v2.pdf 

# Computer vision applications I

<!-- image -->

Object Detection

Penguin

<!-- image -->

Classification

Penguin

<!-- image -->

Semantic segmentation

<!-- image -->

Instance segmentation

# Computer vision applications II

<!-- image -->

Image captioning

<!-- image -->

Style transfer

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

Action recognition

# Computer vision applications III

<!-- image -->

Image generation 

https://thispersondoesnotexist.com/ 

https://edition.cnn.com/2020/02/28/tech/fake-twitter-candidate-2020/index.html 

<!-- image -->

https://thesecatsdonotexist.com/ 

<!-- image -->

<!-- image -->

# Computer vision applications IV

Image translation

<!-- image -->

Talking head models

# Computer vision applications V

<!-- image -->

An astronaut riding a horse in a photorealistic style 

<!-- image -->

A painting of a fox sitting in a field at sunrise in the style of Claude Monet

<!-- image -->

Teddy bears working on new AI research on the moon in the 1980s

# Computer vision applications VI

- Segment anything
- https://segment-anything.com/demo 
- Grounding DINO
- 

<!-- image -->

# Computer vision surpassed human vision

Captcha recognition

<!-- image -->

Face recognition

<!-- image -->

Object recognition

# Dataset &amp; model zoos

Now

Typically you start with a model architecture that someone else has published.

Datasets:

- https://huggingface.co/datasets  (over 7000 datasets)
- https://www.kaggle.com/datasets (over 200k datasets) 
- 

Models:

- https://paperswithcode.com/ 
- https://huggingface.co/models 
- https://pytorch.org/vision/0.8/models.html , https://pytorch.org/serve/model\_zoo.html 