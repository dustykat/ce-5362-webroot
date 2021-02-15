# CE 5362 Lesson 2 Part 1 Modeling Philosophy

- Modeling
    - What is a model?
    - Why model?
- Parsimony
- Modeling Protocol
- Algorithms and Programming

## Modeling

Computational hydraulics aims to provide the engineer with guidance for correct design, construction, and operation of hydraulic works, including closed conduit, open conduit, and porous flow systems. The computational procedures are reasonably complex and typically beyond the limits of hand computation; computer programs are necessary and required for many computational hydraulics problems.

- Modeling is the art of representing reality in a simplified form, sufficient to answer questions about that reality

### What is a Model?

There are many definitions, a few are:

1. A model is a simplification of reality that duplicates the excitation-response of the real system but is faster, smaller, or more practical to study than the real system.

2. A mathematical model is composed of mathematical expressions quantifying fundamental physical principles (force, energy, mass, etc.). These expressions are adapted and simplified in each case to the special features of the problem to be tackled.
3. A model is any device that represents an approximation of a field situation.

There are several categories of models: 

1. **Physical models** such as laboratory sand tanks, flumes, pipe networks, porous columns, batch reactors, and pilot plants are used to directly simulate various conditions anticipated in full scale settings. Physical models tend to be the most costly to build from scratch, but many questions cannot be answered without these kind of model

2. **Analog models** such as electric circuit analogs or Hele-Shaw analogs are used to indirectly simulate field conditions by direct simulation in a domain analogous to the real setting.  Analog computers are quite rare today, but in the 1960's were a principal tool in modeling and were used in aerospace as well as mundane civil engineering applications. A laboratory mouse used to test medicine doses (usually for lethality) is an example of an analog model for human testing. Bear (1972), Chapter 11 provides an excellent background on analog models. The treatment is mostly aimed at porous medium flow, but the concepts are generic. The book has some nice photographs of electric-analog computers (which are rare today).

3. **Mathematical models** indirectly simulate field conditions by solution of a governing equation thought to represent the system, along with auxiliary equations that describe the boundary and forcing conditions. These physics and chemical relationships are represented as sets of algebraic equations. Conversion from physics to the computer is via finite-difference, finite-element, finite-volume analysis. Particle tracking is an alternate approach. Mathematical models are solved analytically or numerically -- both solutions may require a computer.

    - The set of commands used to solve a mathematical problem on a computer is the program or code. 
    - The code is generic, whereas a model is comprised of boundary and initial conditions, a computational grid, material properties on that grid, and forcing terms. 
    - Thus a model is **both** the data and code, whereas the code alone is just a tool waiting for use.

Another kind of model is a statistical (or data model) model that relates observations to excitations without necessarily attempting to relate underlying physics of the problem (Machine Learning; Neural Networks, Regression Trees, Support Vector Machines are these kind of models). 

All the kinds of models are useful and appropriate tools in their various applications.

### Why Model?

There are several good reasons to model, and many poor reasons. 
- Most modeling efforts are attempts to predict the consequences of a proposed design or operation strategy. 
- Models can also be used to interpret system dynamics or as a framework for organizing data. 
- Models can also be used to study processes in generic settings. Generic models are useful in formulating regulatory guidelines and as screening tools to identify suitable or unsuitable designs.

These three modeling applications are fundamentally different -- even though they use the same toolkits. 

Table 1 lists these three application types along with some opinions as to the need to calibrate.

|Table 1. Types of Model Applications|
|---|

|Model Type|Uses|Remarks|
|:---|:---|:---|
|Predictive|Used to predict responses to changes. What if ...?|Requires Calibration|
|Interpretive|Used as a framework for studying system dynamics; framework for organzing data|Calibraton Optional|
|Generic|Analyze behavior in hypothetical systems; useful for framing regulatory/control guidelines|Calibraton Optional|

Models should be investigated and used only if they can answer questions that need to be answered.  
- A list of questions should be prepared prior to beginning a modeling exercise, there are likely some situations where a model may not even be needed. 
- Models should not be expected to “prove" anything -- a computer model cannot prove anything.
- Models are useful to identify poor solutions to a problem, but rarely can they identify best solutions.
- Models can be used to evaluate alternative designs (and this task is a good use of a model). 
- Models can occasionally be used to detect natural relationships that would otherwise go unnoticed.

### Principle of Parsimony

The principle of parsimony is fundamental in engineering modeling. A model should only be as complex as necessary to answer the questions asked of it. All other things being equal a simpler model (fewer parameters) is preferred to a complicated model. This principle is sometimes referred to as Occam's razor.

### A Modeling Protocol

A straightforward modeling protocol can be modeled on the classic "Scientific Method"

1. List the questions that need to be answered -- this step is crucial, it is the problem statement.
2. List the available and necessary data to answer the questions. 

If the data do not exist, that fact should also be noted.

3. Assuming a model is the best tool, list the physical principles that need to be preserved in a model.
4. Develop and test an `algorithm` to simulate the physical principles. In many cases, a professional program will be the tool of choice, although sometimes a “roll-your-own” approach makes better sense. 
    - Prior to actually solving the problem in question, develop and test simple problems to which you know the answer. This exercise develops the modelers skill and builds confidence in the tool.
5. Apply the model working from a simple, known solution towards the problem to which you seek the actual solution. 
    - This step requires keeping a “simulation log." In this log, file names should be recorded (input files should change name each new run  -- record in the log the general nature of each simulation, the output, and the modelers interpretation for each new run)
6. When the real problem is run construct professional exhibits (graphs, tables, etc.) for inclusion in the modeling report. 

Pages of output may be meaningful, but are not sufficient for a professional document.

7. Build the modeling report, including the methods used, inputs and assumptions, outputs (results) and their interpretation in the context of the original problem.

Anderson and Woessner (1992), pages 2 - 10, is an excellent discussion of modeling philosophy and protocol. Their book is also aimed at porous 
flow modeling, but again the concepts are generic. The reader will find these notes borrow (nearly verbatim!) from this reference source.

### Algorithm

An algorithmic procedure can be represented as

$x = f(a) $

From a mathematical perspective the main concern is that the algorithm is well
posed:

   1. A solution exists for a given $a$.
   2. The computation must lead to a single solution for $x$ given $a$.
   3. The results for x must be connected to the input a through the Lipschitz relation: 
   - $|\delta a| < \eta ~\texttt{then}~ |\delta x| < M|\delta a| $ where M is a bounded natural number, $M = M(a;\eta)$. 
   - Many problems are not well posed as stated, but with some reasonable assumptions can be forced into such a state.

Thus an algorithm is a recipe to take input data and produce output responses through
some relationships. If a well posed problem then each result is related to the inputs, and the
same inputs (in an algorithm) produce the same results. By the recipe analogy, if you follow
the same recipe each time with the same raw materials then the cake should taste the same
when it is baked.

An important concept from above is that an algorithm operates on data (a procedure-oriented approach); whereas an object-oriented view is that an algorithm performs a task (generate response) based on states established by the data. Both points of view are valid and equivalent.
Most computational hydraulics models were built (by a quirk of history) in a procedure-
oriented perspective. More recent tools are somewhat of a hybrid creation.

### Tools

A practicing modeler needs a toolkit - these tools range from the actual computation engines (EPA-SWMM, HEC-RAS, FESWMS, HSPF, WSPRO, TR-20, etc.) to analysis tools for result interpretation (Python, R, Excel, etc.) to actual programming tools (Python, R, C, FORTRAN, PERL, etc.) to
construct their own special purpose models or to test results from general purpose profes-
sional models.

In this course, Python will be used (interchangeably where practical) for
programming and analysis. Professional codes (i.e. EPA-SWMM, EFDC, HEC-RAS, etc. will be used later in the course for semi-professional
applications.

### Programming

Why bother with any programming? There are three fundamental reasons for a programming experience in this (or any such) class:
1. Teaching someone else a subject or procedure forces the teacher to have a reasonable understanding of the subject or procedure. Teaching a computer (by virtue of programming) forces a very deep understanding of the underlying algorithm.
2. You will encounter situations that general purpose programs are not designed to address; if you have even only a moderate ability to build your own tools when you need to, then you can. In all likliehood, you will "trick" the professional program, but you cannot invent tricks unless you know a little bit about programming.
3. Programming a computer requires an algorithmic thought process - this process is valuable in many other areas of engineering, hence the act of programming is good discipline for other problems you will encounter.

Why Python?  Admittedly not my first choice, but in keeping with the WCOE emerging emphasis on Computational Thinking and the college's choice to standardize on Python 3.X (as implemented in Jupyter notebooks) that's the tool for this class. 

Because the goal in most cases is to answer a question using the tools, other choices are fine (R, C, C++, FORTRAN90, etc.), but I won't try very hard to help you debug your scripts if they are in R, C or FORTRAN.  

## References

Anderson, M. P. and W. W. Woessner (1992). Applied Groundwater Modeling. San Diego: Academic Press. (pp 2-10)

Bear, J. (1972). Dynamics of Fluids in Porous Media. New York: Dover Publications, Inc. (Chapter 11)


```python

```
