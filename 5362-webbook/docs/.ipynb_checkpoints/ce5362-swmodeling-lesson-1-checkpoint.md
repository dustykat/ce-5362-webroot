# CE 5362 Surface Water Modeling
## Lesson 1: Introduction

These web pages are built using JupyterLab, in most of my pages, there are two special code blocks, the one below to make markup cells properly left-align, and the one at the bottom which identifies the server that last ran the notebook.  You need not understand the script in the cells, just a warning of their presence.


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
### Description: 
The course prepares engineers to practice hydrologic and hydraulic modeling in one-dimensional <strong>unsteady</strong> conditions; and in higher spatial dimensions (2D/3D) using custom-written and professional software tools. Integration of simulation tools to examine complex systems is emphasized.

### Syllabus:

The syllabus is located at [http://54.243.252.9/ce-5362-psuedo-course/0-Syllabus/ce5362-swmodeling-2021-1.html](http://54.243.252.9/ce-5362-psuedo-course/0-Syllabus/ce5362-swmodeling-2021-1.html)

### Content Server(s):
* The primary course content server is [http://54.243.252.9/ce-5362-psuedo-course/](http://54.243.252.9/ce-5362-psuedo-course/)
* The development course content server (used as a backup) is [https://github.com/dustykat/ce-5362-webroot](https://github.com/dustykat/ce-5362-webroot)

### Learning Management System (LMS)
* Blackboard [https://ttu.blackboard.com/webapps/login](https://ttu.blackboard.com/webapps/login)

### Software
1. JupyterLab as implemented in Anaconda for custom-written programs
    1. You can run these on your laptops, or on an AWS Lightsail Instance as you prefer.  
    2. The instructor runs his examples on either an AWS Lightsail Instance (or sometimes on a Raspberry Pi)
    
2. SWMM
    1. You can install and run on your laptops, or on an AWS Lightsail Instance as you prefer.  
    2. The instructor will run SWMM on an AWS Lightsail Instance 
    
3. EFDC and/or SToRM (to be determined as course progresses)
    1. These are clock-cycle hogs; you can install and run on your laptops, or on an AWS Lightsail Instance as you prefer. I am going to use an AWS Lightsail Instance 


## Textbook: 
none, but there are readings located at: [http://54.243.252.9/ce-5362-psuedo-course/3-Readings/](http://54.243.252.9/ce-5362-psuedo-course/3-Readings/) which will be referenced during the course.


---
## Surface Water Modeling - What's the Point?: 

Consider an excerpt from a recent e-mail:

**Surface Water Hydrologist (Entry-Level), Tampa, FL   21-02-TPA01**
 
All interested applicants must apply online at [https://www.interacareers.com/jobDesc.asp?JobID=215]( https://www.interacareers.com/jobDesc.asp?JobID=215)
 
INTERA is seeking a Surface Water / Groundwater Hydrologist who is skilled using and developing modern flow and transport modeling tools in our Tampa, Florida, office. The candidate should have a BS or MS in engineering that focuses on water resources or a related field. The chosen candidate will conduct `data processing`, `analysis`, and `visualization` to support `surface water`,` groundwater`, and `integrated modeling` projects throughout the state of Florida and will develop hydrologic and hydraulic models under the supervision of a professional engineer. 

This is an excellent opportunity to advance your technical skills and career while collaborating with some of the industry’s top talent. As a company wholly dedicated to providing safe and technically sound solutions that exceed our clients’ expectations, we are looking for self-starters who face all project challenges head-on with expertise and commitment.

INTERA’s corporate vision is Delivering Excellence with Every Solution, and we invite all interested candidates to apply if you can embody this on both a personal and professional level. 
 
***Essential Duties and Responsibilities***
This position requires an interest in the design and calibration of `surface water`, groundwater, and `integrated models` and a strong background in programming and geographic information systems (GIS); strong report writing, and communication skills are also essential. The candidate will perform data visualization with large data sets by developing scripts in `Python` or `R` and develop spatial plots of data in `GIS`.  The position will include documentation of all work products in technical reports and presentations.  Additional duties include `developing` models and alternative `simulations` under the supervision of a professional engineer. The candidate must be able to work independently and have experience working with other modelers, engineers, and well-educated clients.
 
***Desired Education and Skills***
- BS or MS in Civil/Environmental Engineering with an emphasis on hydrology or a related field
- Experience analyzing large data sets programmatically using R or Python
- Experience with Geographic Information System (GIS) programs such as ArcMap
- Excellent written and verbal communication skills
- Experience with `HSPF`, `ICPRv4`, `HEC-RAS`, the suite of HEC-RAS tools, and `MODFLOW` is preferred
- Programming using R or Python

***Required Qualifications***
- Must be able to exercise professional judgement, perform QA/QC on data sets, and examine data with a critical eye
- Excellent time management skills in organizing, planning, and prioritizing work
- Must be reliable and conscientious and have the ability to maintain effective and positive work relationships with others, be a cooperative team player and work with a wide variety of people.
- Excellent verbal and written communication skills are required

***About INTERA & Our Benefits***

Our employees say that what they value most at INTERA is our corporate culture—because not only do we believe that our people are our most important asset, we embody it. As an employee-owned company, we are invested in not only providing a good work-life balance with flexible schedules and telecommuting opportunities, but we offer generous benefits packages (we even have a 0-dollar cost health plan), bonus eligibilities, tuition reimbursements, and more. These are some of the reasons why many of our employees have spent 10 to 25 years or more at INTERA. We love what we do and it shows.

So join us today because INTERA has a reputation of excellence, leaves no room for politics, and empowers all of our employees to accomplish their best!

## The Point

There are jobs in the field, so you should have some background in the general topic -- notice the advertisement mentions specific software -- we will use similar software to explore the concepts in these commercial products; its not hard to translate skills - but we have to have them in the first place!

There is a fair amount of on-line how-to [https://www.epa.gov/waterdata/surface-water-quality-modeling-training](https://www.epa.gov/waterdata/surface-water-quality-modeling-training) but this alone just makes you dangerous, here we will try to get an understanding of the complexity of the models, so we can trick them as needed (trick in the good sense; make them work for us!)

---
## What is Modeling?

Modeling is the art of representing reality in a simplified form, sufficient to answer questions about that reality

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

---
## Lesson 1: Workshop

Getting us into a common computing environment:

I am going to use **Amazon Web Services** Lightsail (Virtual Private Server) Instances: They are low cost (~24 USD/month or less) compute servers for running models on a remote AWS maintained machine.   You can try AWS yourself, or do everything on a laptop.  If it's a Apple Macintosh you might have a better experience with AWS.

### Install a Remote Desktop Protocol Client
You will use a Windows Server environment on your AWS instance.  You need a RDP client to connect to the server and get a GUI. If you have Windows RDP is built-in, if you are a Mac or Linux user you will need to obtain a useable client. The workshop will demonstrate using a Macintosh, then using a Raspberry Pi running Ubuntu Linux to demonstrate the procedure.

### Build an AWS Lightsail Instance/Install Anaconda

I already made a video on this part, so I will just play it [https://www.youtube.com/watch?v=s8CJKq2Rvgg&feature=youtu.be](https://www.youtube.com/watch?v=s8CJKq2Rvgg&feature=youtu.be)

### Install SWMM 
I will do this live, so I have a current copy (also to see if it works on AWS as expected).

---
The other special script block is below!


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

