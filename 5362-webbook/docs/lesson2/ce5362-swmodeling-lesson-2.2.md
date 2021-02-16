# CE 5362 Lesson 2 Hydraulics Review

- Open Channel Hydraulics (A Review)
    - 1D Spatial: St. Venant Equations 
    - Gradually Varied Flow
    - Water Surface Profiles by Variable Step
    - Water Surface Profiles by Fixed Step

## St. Venant Equations

In general, 1-D unsteady flow would be considered state-of-practice computation; every engineer would be expected to be able to make such calculations (using professional software). 2-D computation is becoming routine using general purpose software. 3-D computation as of this writing (circa 2009) is still in the realm of state-of-art, and may not be within the capability a typical consulting firm.

The conservation of mass, momentum, and energy in the context of the cell balance method is used herein to develop the mathematical and computational structure. The cell balance is a computational structure that is somewhat analogous to the Reynolds transport theorem, except the end results are difference equations that can be updates to approximate physical processes. 

The philosophy is a hybrid approach - instead of developing the differential equations first, then numerical approximations, the numerical constructs are built directly and the limiting process is employed to demonstrate that the constructs indeed mimic the differential equations that describe our current understanding of the physics.

### The Computational Cell

The cell balance method envisions the world as representable by a computational cell (or more typically a collection of cells) with some finite dimension, fixed in space about a cell centroid. Some dimensions are changeable - such as depth.

The fundamental computational element is a computational cell or a reach. Some professional software, in particular HEC-RAS, considers a reach to be a specific portion of a river system that may be comprised of several computational sub-reaches (cells). The engineer will need to consider the context and the tool used to decide which way to describe their problem to the program (and to the client).

Figure 1 is a sketch of a portion of a channel. The left-most section is uphill (and upstream)
of the right-most section. The section geometry is arbitrary, but is drawn to look like
a channel cross section.

![](reach_sketch.png)

|**Figure 1 Reach/Computational Cell**|
|---|


The length of the reach (distance between each section along the flow path) is $\Delta x$.
The depth of liquid in the section is $z$, the width at the free surface is $B(z)$, the functional relationship established by the channel geometry. The flow into the reach
on the upstream face is $Q -\frac{\partial Q}{\partial x}\frac{\Delta x}{2}$ The flow out of the reach on the downstream face
$Q +\frac{\partial Q}{\partial x}\frac{\Delta x}{2}$. The direction is strictly a sign convention and the development does not require flow in a single direction. The topographic slope is $S_0$, assumed relatively constant in each reach, but can vary between reaches.

The development of the unsteady flow equations herein uses several assumptions:
1. The pressure distribution at any section is hydrostatic ; this assumption allows computation of pressure force as a function of depth.
2. Wavelengths are long relative to flow depth; use the shallow wave theory.
3. Channel slopes are small enough so that the topographic slope is roughly equal to the tangent of the angle formed by the channel bottom and the horizontal.
4. The flow is one-dimensional ; this assumption implies that longitudinal dimension is large relative to cross sectional dimension. Generally river flows will meet this assumption, it fails in estuaries where the spatial dimensions (length and width) are roughly equal. Thus rivers that are hundreds of feet wide imply that reaches are miles long. If this assumption cannot be met, then 2-D methods are more appropriate.'
5. Friction is modeled by Chezy-Manning type empirical models. The particular friction model does not really matter, but historically these equations have used the friction slope concept as computed from one of these empirical models.

The tools that are used to build the equations are conservation of mass and linear
momentum.

### Conservation of Mass

The conservation of mass in the cell is the statement that mass entering and leaving
the cell is balanced by the accumulation or lass of mass within the cell. For pedagog-
ical clarity, this section goes through each part of a mass balance then assembles into
a difference equation of interest.

**Mass Entering**: Mass enters from the left of the cell in our sketch. This direction only establishes a direction convention and negative flux means the arrow points in the direction opposite of that in the sketch. In the notation of the sketch mass entering in a short time interval is:

$$ \dot M_{in} = \rho \cdot (Q -\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t$$

**Mass Exiting**: Mass leaves from the right of the cell in our sketch. In the notation
of the sketch mass leaving is:

$$ \dot M_{in} = \rho \cdot (Q +\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t$$

**Mass Accumulating**: Mass accumulating within the reach is stored in the prism depicted in the sketch by the dashed lines. The product of density and prism volume is the mass added to (or removed from) storage.

The rise in water surface in a short time interval is $\frac{\partial z}{\partial t} \cdot \Delta t$ The plan view area of the prism is $B(z) \cdot \Delta x$. The product of these two terms is the mass added to storage, expressed as:

$$ \dot M_{storage} = \rho \cdot (\frac{\partial z}{\partial t} \cdot \Delta t) \cdot B(z) \cdot \Delta x$$

Equating the accumulation to the net inflow produces

$$\rho \cdot (\frac{\partial z}{\partial t} \cdot \Delta t) \cdot B(z) \cdot \Delta x = \rho \cdot (Q -\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t - \rho \cdot (Q +\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t$$

This is the mass balance equation for the reach. If the flow is isothermal, and essentially incompressible then the density is a constant and can be removed from both sides of the equation. 

$$(\frac{\partial z}{\partial t} \cdot \Delta t) \cdot B(z) \cdot \Delta x =   (Q -\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t -  (Q +\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t$$

Remove the $+Q$ and $-Q$ and simplify the right hand side

$$(\frac{\partial z}{\partial t} \cdot \Delta t) \cdot B(z) \cdot \Delta x =  - (\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t -  (\frac{\partial Q}{\partial x}\frac{\Delta x}{2}) \cdot \Delta t = - \frac{\partial Q}{\partial x} \cdot {\Delta x} \cdot \Delta t$$

Dividing both sides by $\Delta x \cdot \Delta t$ yields

$$(\frac{\partial z}{\partial t} ) \cdot B(z)  =  - \frac{\partial Q}{\partial x}  $$

This equation is the conventional representation of the conservation of mass in 1-D
open channel flow. If the equation includes lateral inflow the equation is adjusted to
include this additional mass term. The usual lateral in flow is treated as a discharge
per unit length added into the mass balance as expressed as:

$$(\frac{\partial z}{\partial t} ) \cdot B(z) + \frac{\partial Q}{\partial x}  = q$$

This last equation is one of the two equations that comprise the St. Venant equations.
The other equation is developed from the conservation of linear momentum.

### Conservation of Momentum

The conservation of momentum is the statement of the change in momentum in the
reach is equal to the net momentum entering the reach plus the sum of the forces on
the water in the reach. As in the mass balance, each component will be considered
separately for pedagogical clarity.

Figure 2 is a sketch of the reach element under consideration, on some non-zero sloped
surface.

![](motion_sketch1.png)

|**Figure 2 Equation of Motion Sketch**|
|---|

**Momentum Entering**: Momentum entering on the left side of the sketch is:

$$ \rho \cdot QV = \rho \cdot V^2A $$

**Momentum Exiting**: Momentum leaving on the right side of the sketch is:

$$ \rho \cdot QV + \frac{\partial}{\partial x}(\rho \cdot QV)\delta x = \rho \cdot V^2A + \frac{\partial}{\partial x}(\rho \cdot V^2A)\delta x$$

**Momentum Accumulating**: The momentum accumulating is the rate of change of
`linear` momentum:

$$ \frac{dL}{dt} = \frac{d(mV)}{dt} = \frac{\partial}{\partial t} (\rho \cdot AV \cdot \delta x)=\rho \cdot \delta x \cdot \frac{\partial}{\partial t} (AV) $$

**Gravity Force**: The gravitational force on the element is the product of the mass in
the element and the downslope component of acceleration.  The mass in the element is $\rho A \delta x$ The x-component of acceleration is $g sin(\alpha)$, which is $\approx S_0$ for small values of
$ \alpha$.  The resulting force of gravity is is the product of these two values:

$$F_g = \rho g AS_0 \delta x$$

**Friction Force**: Friction force is the product of the shear stress and the contact area.
In the reach the contact area is the product of the reach length and average wetted
perimeter.

$$F_{f} = \tau P_w \delta x$$

where $Pw = A/R$, where $R$ is the hydraulic radius. A good approximation for shear stress in unsteady flow is $\tau = \rho g R S_f$. The value $S_f$ is the slope of the energy grade line at some instant and is also called the friction slope. This slope can be empirically determined by a variety of models, typically Chezy's or Manning's equation is used. In either of these two models, we are using the STEADY FLOW equation of motion to mimic unsteady behavior - nothing wrong, and it is common practice, but this decision does limit the frequency response of the model (the ability to change fast - hence the shallow wave theory assumption!).

The resulting friction model is

$$F_{f} = \rho g A S_f \delta x$$
**Pressure Force**: The pressure forces are comprized of the pressure applied at the end faces of the reach, and along the channel sides.  Figure 3 is a sketch that depicts a panel of a cross section that is integrated vertically to obtain pressure magnitude. 

![](pressure_sketch.png)

|**Figure 3 Pressure-Integral Sketch**|
|---|

$$ F_p = \int_A dF$$

where 

$$ dF = (z-h)\rho g \xi (h) dh$$

where $\xi (h)$ is the width panel at a given distance above the channel bottom
$(h)$ at any section.

The net forceis the differnece in the upstream and downstream forces:
$$ F_{p~net} = F_{p~up} - F_{p~down} = F_p - (F_p + \frac{\partial F_p}{\partial x}\cdot \delta x) = - \frac{\partial F_p}{\partial x}\cdot \delta x $$

when expressedusing the integral:
$$ - \frac{\partial F_p}{\partial x}\cdot \delta x  = - \frac{\partial}{\partial x}[\int_0^Z \rho g (z-h) \xi(h) dh]\delta x$$

evaluating by parts
$$ F_{p~net} = - \rho g [\frac{\partial z}{\partial x}\int_0^Z \xi(h) dh + \int_0^Z (z-h) \xi(h) \frac{\partial \xi(h)}{\partial x} dh]\delta x $$

The first term integrates to the cross sectional area, the second term is the variation
in pressure with position along the channel. The other pressure force to consider is the bank force (the pressure force exerted by the banks on the element). This force is computed using the same type of integral structure except the integrand order is swapped.

$$ F_{p~bank} = [\int_0^Z (z-h) \xi(h) \frac{\partial \xi(h)}{\partial x} \delta x]dh $$

Now to combine the parts into a momentum balance:

$$ Momentum_{in} - Momentum_{out} + \sum F = \frac{d(mV)}{dt} $$

Substitution of the pieces :

$$ Momentum_{in} - Momentum_{out} + F_{p~net} + F_{bank} + F_{gravity} - F_{friction} = \frac{d(mV)}{dt} $$

Now write the expressions for each expressions for each part:

$$ \rho \cdot V^2A - \rho \cdot V^2A - \frac{\partial}{\partial x}(\rho \cdot V^2A)\delta x \\ - \rho g [\frac{\partial z}{\partial x}\int_0^Z \xi(h) dh - \int_0^Z (z-h) \xi(h) \frac{\partial \xi(h)}{\partial x} dh]\delta x \\ + [\int_0^Z (z-h) \xi(h) \frac{\partial \xi(h)}{\partial x} \delta x]dh \\ + \rho g AS_0 \delta x \\ - \rho g A S_f \delta x \\ = \rho \cdot \delta x \cdot \frac{\partial}{\partial t} (AV) $$

The rows above, in order represent:
1. Net momentum entering the reach.
2. Pressure force differential at the end sections.
3. Pressure force on the channel sides.
4. Gravitational force.
5. Frictional force opposing flow.
6. Total acceleration in the reach (as change in linear momentum). 


### Simplifications

Canceling terms and dividing by $\rho \delta x$ (isothermal, incompressible flow; reach has finite
length), The momentum equation simplifies to:

$$-\frac{\partial }{\partial x}(V^2 A) - g \frac{\partial z}{\partial x}\int_0^Z \xi(h) dh + g A S_0 - g R S_f = \frac{\partial }{\partial t}(AV)$$

The second term integral is the sectional flow area, so it simplifies to

$$-\frac{\partial }{\partial x}(V^2 A) - g \frac{\partial z}{\partial x}A + g A S_0 - g A S_f = \frac{\partial }{\partial t}(AV)$$

The term with the square of mean section velocity is expanded by the chain rule ([https://www.youtube.com/watch?v=H-ybCx8gt-8](https://www.youtube.com/watch?v=H-ybCx8gt-8)), and using **continunity** becomes (notice the convective acceleration term from the change in area with time)

$$-\frac{\partial }{\partial t}(AV) = A \frac{\partial V}{\partial t}  + V \frac{\partial A}{\partial t} = A \frac{\partial V}{\partial t} - VA \frac{\partial V }{\partial x} - V^2 \frac{\partial A}{\partial x}$$

Now expand and construct

$$-V^2 \frac{\partial A}{\partial x} -2VA \frac{\partial V}{\partial x} - g A \frac{\partial z}{\partial x} + g A (S_0 - S_f) = A \frac{\partial V}{\partial t} - VA \frac{\partial V}{\partial x} - V^2 \frac{\partial A}{\partial x}$$

Cancel common terms and simplify

$$ -VA \frac{\partial V}{\partial x} - g A \frac{\partial z}{\partial x} + g A (S_0 - S_f) = A \frac{\partial V}{\partial t}$$

The above equation is the form of the momentum equation for practical use. It will be
rearranged in the remainder of this essay to fit some other purposes, but this is the
expression of momentum in the channel reach.

A bit more manipulation (divide by $gA$)

$$ -\frac{V}{g}\frac{\partial V}{\partial x} - \frac{\partial z}{\partial x} + (S_0 - S_f) = \frac{1}{g} \frac{\partial V}{\partial t}$$

Now rearrange to place the two slopes on the left side, and the remaining part of
momentum to the right side. The result let's us examine the several flow regimes
common in open channel flows.

$$ S_0 - S_f = \frac{1}{g} \frac{\partial V}{\partial t} + \frac{\partial z}{\partial x} + \frac{V}{g}\frac{\partial V}{\partial x} $$

If the local acceleration (first term on the right) is zero, the depth taper (middle term on the right) is zero, and the convective acceleration (last term on the right) is zero, then the expression degenerates to the algebraic equation of normal flow ($S_0 = S_f$). If just the local acceleration term is zero, and all the remaining terms are considered, then the expression degenerates to the ordinary differential equation of gradually varied flow. Finally, if all the terms are retained, then the dynamic flow (shallow wave) conditions are in effect and the resulting model is a partial differential equation.

### Flow Regimes

Re-iterating these typical 
ow regimes.
1. Uniform flow; algebraic equation.
$$ S_f = S_0 $$

2. Gradually varied; ordinary differential equation.
$$ S_f = S_0 - \frac{\partial z}{\partial x} - \frac{V}{g}\frac{\partial V}{\partial x} $$

3. Dynamic flow (shallow wave) conditions; partial differential equation.

$$ S_f = S_0 - \frac{1}{g} \frac{\partial V}{\partial t} - \frac{\partial z}{\partial x} - \frac{V}{g}\frac{\partial V}{\partial x} $$

The coupled pair of equations, for continuity and for momentum are called the St. Venant equations and comprise a coupled hyperbolic differential equation system.

A. Continunity $$(\frac{\partial z}{\partial t} ) \cdot B(z) + \frac{\partial Q}{\partial x}  = q$$

B. Momentum $$ S_0 - S_f = \frac{1}{g} \frac{\partial V}{\partial t} + \frac{\partial z}{\partial x} + \frac{V}{g}\frac{\partial V}{\partial x} $$

Solutions ($(z, t)$ and $(V, t)$ functions) are found by a variety of methods including
finite difference, finite element, finite volume, and characteristics methods.
In the next lesson we will examine solutions to the gradually varied flow equation, then
proceed to a finite difference solution to the full dynamic equations in the following
chapter.

## References

Koutitas, C. G. 1983. Elements of Computational Hydraulics. ISBN 0-412-00361-9. Pentech Press Ltd. London. (Chapter 4) [](https://atomickitty.ddns.net/antares/3-Readings/PracticalComputationalHydraulics-Kousitas/ElementsOfComputationalHydraulics.pdf)

Roberson, J. A., Cassidy, J.J., and Chaudry, M. H., (1988) Hydraulic Engineering, Houghton Mifflin Co (Chapter 10) [](https://atomickitty.ddns.net/antares/3-Readings/HydraulicEngineering/Chapter%2010/CHapter%2010.pdf)

Sturm T.W (2001) Open Channel Hydraulics, 1ed., McGraw-Hill, New York. Note: This PDF is from an international edition published in Singapore. [](https://atomickitty.ddns.net/antares/3-Readings/OpenChannelHYdraulics/open-channel-hydraulics-sturm.pdf)


```python

```
