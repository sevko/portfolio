# chapter 2: forces
A *force* is a vector that causes an object with mass to accelerate. The absence or perfect cancellation of forces is
called *equillibrium*.

$$
\vec{F} = m \vec{a}
$$

The *mass* of an object is a quantification of its matter. *Weight* is the force exerted on a mass by gravity ($weight =
m \vec{g}$). *Density* is mass per unit volume.

## friction
Friction is a *dissipative force*.

$$
F_{friction} = - \hat{v}muN
$$

With:

  * $v$: the object's velocity.
  * $N$: the force exerted on the object by the surface it's situated on.
  * $\mu$: the surface's **coefficient of friction**.

## air and fluid resistance
*Viscuous force*, *drag force*, or *fluid resistance* are all synonyms for friction exerted by a liquid or gas.

$$
F_d = -\frac{1}{2}pv^2Ac_d \hat{v}
$$

With:

  * $\rho$: the density of the medium (**not important**).
  * $v$: the velocity of the object.
  * $A$: frontal area of the object (**not important**).
  * $c_d$: the *coefficient of drag*, analogous to the *coefficient of friction*.

For the purposes of naive simulations, the following formula will suffice:

$$
f_d = -\hat{v} \|\vec{v}\|^2c_d
$$

## gravity
$$
F_g = \frac{G m_1 m_2}{r^2} \hat{r}
$$
