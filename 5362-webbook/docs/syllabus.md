# CE 5362 Surface Water Modeling

Spring 2021

## Catalog Description: 
Theory and application of hydrodynamic and mass transport models.  Representative application of models for watershed and/or estuary modeling of flow and constituent transport.

3 credit hours comprising of lecture and hands-on workshop/exercise sessions.

## Course Overview

The course trains engineers to practice hydrologic and hydraulic modeling in one-dimensional <strong>unsteady</strong> conditions; and in higher spatial dimensions (2D/3D) using custom-written and professional software tools. Integration of simulation tools to examine complex systems is emphasized.

## Prerequisites: 

## COVID-19 Important Guidelines:
* If Texas Tech University campus operations are required to change because of health concerns related to the COVID-19 pandemic, it is possible that this course will move to a fully online delivery format.  Should that be necessary, students will be advised of technical and/or equipment requirements, including remote proctoring software.  

* Policy on absences resulting from illness: We anticipate that some students may have extended absences.  To avoid students feeling compelled to attend in-person class periods when having symptoms or feeling unwell, a standard policy is provided that holds students harmless for illness-related absences (see Section A below).


### A. Illness-Based Absence Policy (Face-to-Face Classes)
If at any time during the semester you are ill, in the interest of your own health and safety as well as the health and safety of your instructors and classmates, you are encouraged not to attend face-to-face class meetings or events.  Please review the steps outlined below that you should follow to ensure your absence for illness will be excused.  These steps also apply to not participating in synchronous online class meetings if you feel too ill to do so and missing specified assignment due dates in asynchronous online classes because of illness.

1. If you are ill and think the symptoms might be COVID-19-related:

    1. Call Student Health Services at 806.743.2848 or your health care provider.  During after-hours and on weekends, contact TTU COVID-19 Helpline at TBD.
    2. Self-report as soon as possible using the Dean of Students COVID-19 webpage. This website has specific directions about how to upload documentation from a medical provider and what will happen if your illness renders you unable to participate in classes for more than one week.
    3. If your illness is determined to be COVID-19-related, all remaining documentation and communication will be handled through the Office of the Dean of Students, including notification of your instructors of the time you may be absent from and may return to classes.
    4. If your illness is determined not to be COVID-19-related, please follow steps 2.a-d below.


2. If you are ill and can attribute your symptoms to something other than COVID-19:

    1. If your illness renders you unable to attend face-to-face classes, participate in synchronous online classes, or miss specified assignment due dates in asynchronous online classes, you are encouraged to contact either Student Health Services at 806.743.2848 or your health care provider.  Note that Student Health Services and your own and other health care providers may arrange virtual visits.
    2. During the health provider visit, request a “return to school” note.
    3. E-mail the instructor a picture of that note.
    4. Return to class by the next class period after the date indicated on your note.

Following the steps outlined above helps to keep your instructors informed about your absences and ensures your absence or missing an assignment due date because of illness will be marked excused.  You will still be responsible to complete within a week of returning to class any assignments, quizzes, or exams you miss because of illness.

### B. Illness-Based Absence Policy (Telepresence Classes)
Same as above with respect potential to infect others; go to a health care provider if you are ill.  Telepresence courses are recorded and will be available on TTU MediaSite and/or YouTube (unlisted).  Exercises, Quizzes, and Examinations are all administered by a Learning Management System (Blackboard) and users need to allow enough time to complete and upload their work>

## Course Sections
Lesson time, days, and location: 

1. Section D01; CRN ?????; 0800PM-0920PM M, W ; Telepresence (Zoom)
1. Section 010; CRN ?????; 0800PM-0920PM M, W ; Telepresence (Zoom)

Please note the times are selected so that international students can attend at a reasonable time of day.

0800PM LBB == 0800AM BST (next day) thus

0800PM M LBB == 0800PM T BST
0800PM W LBB == 0800PM TH BST

## Course Instructor:

Instructor: Theodore G. Cleveland, Ph.D., P.E., M. ASCE, F. EWRI

Email: theodore.cleveland@ttu.edu  (put ENGR 5362 in subject line for email related to this class)

Office location: Telepresence (Zoom; GoToMeeting; etc.)

Office hours: TBD

## Teaching assistants: 
Email : none

Office location: none

Office hours: none

## Textbook: 
none

## Course Contents: 

* Modeling Philosophy
* Hydraulics:
    * 1-D Dynamic Wave Routing:
        1. Lax-Diffusion (Homebrew using JupyterLab/iPython)
        2. SWMM 
        3. 1-D Branched Systems:
            1. Simulate the confluence of two streams in steady conditions using SWMM
            2. Simulate the confluence of two streams in unsteady conditions using SWMM
    * 2-D Floodplain Approximation
        1. Interconnected Channel and Pond Routing (ICPR): 
            1. Conceptualization
            2. Storage elements
            3. Routing elements, 
            4. Approximations using SWMM.
* Hydrology:
    * Subcatchment Representation: 
        1. Shape and Scale,
        2. Runoff Generation
* Integrated H&H Models:
	* Subdivision scale (ICPR concept);
    * County scale (Branched 1D) ;
* Constituient Transport (Water Quality);
    1. Tracers, 
    2. Reactive Transport, 
    3. Green Infrastructure (GI) for WQ Enhancement
        1. Infiltration approaches
        2. Storage-Delay approaches
        3. Filtration approaches
* Advanced Topics (Time Permitting):
    1. EFCD for 2D Hydrodynamics
    2. SToRM for 2D Hydrodynamics
    3. Lattice-Boltzman Approach for 2D/3D hydrodynamics

## Learning Outcomes: 
On completion of the course, students will be able to:
1. Articulate relevant theory for 1D dynamic wave routing.
2. Develop a Lax-Diffusion model for time-varying flow in an open channel and compare results with professional tools (SWMM).
3. Simulate the confluence of two streams in steady and unsteady conditions using professional tools (SWMM).
4. Articulate ICPR concept for approximating 2D estuary/floodplains
5. Employ the ICPR concept for approximating 2D estuary/floodplains
6. Approximate constituient transport in surface water systems
7. Articulate green infrastructure approaches to water quality enhancement
8. Articulate awareness of alternative modeling approaches (Lattice-Boltzman Fluid Dynamics ...)

## ABET Student Outcomes
* Engineering:
    1. An ability to identify, formulate, and solve complex engineering problems by applying principles of engineering, science, and mathematics.
    2. An ability to acquire and apply new knowledge as needed, using appropriate learning strategies. 
    
The script block below is for table formatting no need to understand it.


```python
%%html
<style>
  table {margin-left: 0 !important;}
</style>
```


<style>
  table {margin-left: 0 !important;}
</style>



---
---
## Course Schedule 

| Item | Lesson | Workshop/Exercises |
|:-----|:---|:---|
||<strong>Tools</strong>||
|25 Jan 2021|1 Introduction: <br> - Syllabus <br> - Content Servers <br> - LMS Server <br> - Software | Computing Environment set up: <br> - Installing Anaconda (Win/MacOS/AWS) <br> – Jupyter notebooks <br> - Simple Examples |
|27Jan2021|2 Modeling Philosophy: <br> - Why model <br> - Parsimony <br> |  |
|28 Jan 2021|Mass Conservation: <br> - Computational Cell <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
||<strong>1D Open Channel Hydraulics</strong>||
|1 Feb 2021|4 Mass Conservation: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|3 Feb 2021|5 Momentum Conservation: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|8 Feb 2021|6 Steady Flow Algorithms: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|10 Feb 2021|7 Steady Flow Codes : <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|15 Feb 2021|8 Steady Flow in SWMM: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|17 Feb 2021|9 Confluence (Wurbs and James): <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|22 Feb 2021|10 Unsteady Flow Algorithms: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|24 Feb 2021|11 Unsteady Flow Code (homebrew): <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|1 Mar 2021|12 Unsteady Flow Code (homebrew): <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|3 Mar 2021|13 Unsteady Flow in SWMM: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|8 Mar 2021|14 Confluence : (Wang et. al.): <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|10 Mar 2021|16 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|15 Mar 2021|17 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|17 Mar 2021|18 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|22 Mar 2021|19 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|24 Mar 2021|20 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|29 Mar 2021|21 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
||<strong> placeholder </strong>||
|31 Mar 2021|22 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|5 Apr 2021|23 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|7 Apr 2021|24 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|12 Apr 2021|25 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
||<strong> placeholder </strong>||
|14 Apr 2021|26 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|19 Apr 2021|27 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|21 Apr 2021|28 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|26 Apr 2021|29 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|28 Apr 2021|30 topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|3 May 2021|31topic ...: <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 | laboratory <br> - subtopic1 <br> - subtopic2 <br> - subtopic3 |
|10 May 2021|<strong>Final Project Report and Link to Video</strong>||

---
## Assessment Instruments and Grading Criteria:
There will be an exam and several quizzes and homeworks/project for the course.  

Grades will be based on the following components; weighting is approximate:

|Assessment Instrument|Total points|Weight(%)|
|---|---:|---:|
|Exams/Quizzes|25|
|Literature Review|25|
|Homeworks/Project|50|
|Overall total|100|

Letter grades will be assigned using the following proportions:

|Normalized Score Range|Letter Grade|
|:-|---:|
|≥ 85|A|
|75-84|B|
|65-74|C|
|55-64|D|
|< 55|F|

---
## Resources/Tools
### Amazon Web Services
1. Lightsail (Virtual Private Server) Instances: Low cost (~24 USD/month or less) compute servers for running models on a remote AWS maintained machine. This is what the instructor will use for <strong>Jupyter notebooks, and the various professional tools.</strong>

### Additional Platforms for Python 

1. Anaconda platform (https://www.anaconda.com/): Anaconda distribution is an open-source Data Science Distribution Development Platform.  It includes Python 3 with over 1,500 data science packages making it easy to manage libraries and dependencies.  Available in Linux (x86-64 only), Windows, and Mac OS X.  

2. Jupyter (https://jupyter.org/): JupyterLab is a web-based interactive development environment for Jupyter notebooks, code, and data. JupyterLab is flexible: Configure and arrange the user interface to support a wide range of workflows in data science, scientific computing, and machine learning. `note` Anaconda for MacOS includes a JupyterLab instance, so a separate install is not required.

#### Additional Modules
3. Math module (https://docs.python.org/3/library/math.html): Gives access to the mathematical functions defined by the C standard e.g. factorial, gcd, exponential, logarithm. 
4. Operator module (https://docs.python.org/3/library/operator.html): Helps in exporting a set of efficient functions corresponding to the intrinsic operators of Python.  For example, the operator add(x,y) is equivalent to the expression x+y.
5. Scipy module (https://www.scipy.org/): A Python-based ecosystem of open-source software for mathematics, science, and engineering. Some of the core packages are:
    * Numpy: Provides n-dimensional array package
    * Scipy: Fundamental for scientific computing (e.g. linear algorithm, optimization)
    * Matplotlib: Visualizations/2D plotting
    * IPython: Enhanced interactive console <<= this is the kernel used in JupyterLab
    * Pandas: Data structures and data analysis
6. Scikit-learn module (https://scikit-learn.org/stable/): A library for machine learning in Python. It is a simple and efficient tool for predictive data analysis.  It is built on NumPy, SciPy, and matplotlib modules.

JupyterLab hardware requirements are minimal, in fact this syllabus was initially created using a JupyterLab notebook (as a markdown processor) on a RaspberryPi.

---
## Classroom Policy:
The following activities are not allowed in the classroom: Texting or talking on the cellphone or other electronic devices, and reading non-course related materials.
### Telepresence (On-line) Courses
Obviously electronic devices are vital; disrupting the webinar is prohibited, please mute your microphone unless you have a question - consider typing your question into the chat window as well. 


## ADA Statement: 
Any student who, because of a disability, may require special arrangements in order to meet the course requirements should contact the instructor as soon as possible to make necessary arrangements.  Students must present appropriate verification from Student Disability Services during the instructor's office hours.  Please note that instructors are not allowed to provide classroom accommodation to a student until appropriate verification from Student Disability Services has been provided.  For additional information, please contact Student Disability Services 
office in 335 West Hall or call 806.742.2405.

## Academic Integrity Statement:
Academic integrity is taking responsibility for one’s own class and/or course work, being individually accountable, and demonstrating intellectual honesty and ethical behavior.  Academic integrity is a personal choice to abide by the standards of intellectual honesty and responsibility.  Because education is a shared effort to achieve learning through the exchange of ideas, students, faculty, and staff have the collective responsibility to build mutual trust and respect.  Ethical behavior and independent thought are essential for the highest level of academic achievement, which then must be measured.  Academic achievement includes scholarship, teaching, and learning, all of which are shared endeavors.  Grades are a device used to quantify the successful accumulation of knowledge through learning.  Adhering to the standards of academic integrity ensures grades are earned honestly.  Academic integrity is the foundation upon which students, faculty, and staff build their educational and professional careers.  [Texas Tech University (“University”) Quality Enhancement Plan, Academic Integrity Task Force, 2010].

## Religious Holy Day Statement: 
“Religious holy day” means a holy day observed by a religion whose places of worship are exempt from property taxation under Texas Tax Code §11.20.  A student who intends to observe a religious holy day should make that intention known to the instructor prior to the absence.  A student who is absent from classes for the observance of a religious holy day shall be allowed to take an examination or complete an assignment scheduled for that day within a reasonable time after the absence.  A student who is excused may not be penalized for the absence; however, the instructor may respond appropriately if the student fails to complete the assignment satisfactorily.

## Ethical Conduct Policy:
Cheating is prohibited, and the representation of the work of another person as your own will be grounds for receiving a failing grade in the course.

---
---
The script block below identifies server last used to render notebook


```python
# Script block to identify host, user, and kernel
import sys
! hostname
! whoami
print(sys.executable)
print(sys.version)
print(sys.version_info)
# Script block to left align Markdown Tables
```

    atomickitty.aws
    antares
    /opt/jupyterhub/bin/python3
    3.6.9 (default, Oct  8 2020, 12:12:24) 
    [GCC 8.4.0]
    sys.version_info(major=3, minor=6, micro=9, releaselevel='final', serial=0)



```python

```
