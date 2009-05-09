<TeXmacs|1.0.6.15>

<style|generic>

<\body>
  <section|Introduction>

  In software, we occasionally need to model objects which <em|depend> on
  other objects; a common example is that of the package management system of
  your favorite distribution. Packages must declare the names and versions of
  other packages upon which they depend, and those packages must be installed
  before proceeding. This concept comes up in other areas, and in this short
  document I'm going to lay out some formalisms for generalizing the notion
  of objects with dependency relationships.

  Ultimately, I'd aim to implement these formalisms in the <em|Haskell>
  programming language. Haskell's type system is rich enough to provide a
  foundation for a generic representation of object dependencies, so you can
  decide what ``a depends on b'' means. I assume a relatively simple model of
  dependency here; in some cases,\ 

  To discuss these concepts I'll use some terms to refer to parts of the
  object-dependency system:

  <\itemize-dot>
    <item><strong|object>: an entity which has dependency relationships with
    other objects.

    <item><strong|archive>: the collection of objects available for
    consideration; for example, an archive might consist of all available
    packages for a distribution of your operating system.

    <item><strong|environment>: the context in which these objects are being
    used; for example, your local operating system installation. In
    particular, the environment is the collection of currently installed
    packages.
  </itemize-dot>

  <section|Definitions>

  Let <math|A> represent an <em|archive>. Let <math|E\<subseteq\>A> represent
  an <em|environment>.

  Let <math|\<gg\> : A \<times\> A> be a relation where <math|a \<gg\> b>
  means, for <math|a\<neq\>b>, <math|a> <em|depends on> <math|b>.

  Let <math|D:A\<rightarrow\>\<cal-P\>(A)> be an object's <em|direct
  dependencies>, i.e., <math|D(a) = { b \<in\> A \| a \<gg\> b}>.

  Let <math|F:A\<rightarrow\>\<cal-P\>(A)> be an object's <em|full>
  <em|dependencies>, i.e.,

  <\equation*>
    F(a) =D(a) \<cup\><left|(><big|cup><rsub|b\<in\>D(a)>F(b)<right|)>
  </equation*>

  Let <math|\<sim\>:\<cal-P\>(A)\<times\>A> be a relation where
  <math|E\<sim\>a> means that an environment <math|E> <em|satisfies> the
  dependencies of an object <math|a>; then <math|E \<sim\>a> iff <math|D(a)
  \<subseteq\> E>. An environment may only <em|contain> objects whose
  dependencies it satisfies, i.e., <math|(\<forall\>a\<in\>E\|E\<sim\>a)>.
  Conversely, <math|E\<nsim\>a> means that <math|E> does not satisfy the
  dependencies of <math|a>.

  Let the <em|resolution> <math|R:\<cal-P\>(A)\<times\>A\<rightarrow\>\<cal-P\>(A)>
  be the minimal set of objects, together with <math|E>, that produce an
  environment that satisfies an object's dependencies, i.e.,

  <\equation*>
    R(E,a)=F(a)-E
  </equation*>

  <\equation*>
    [E\<cup\>R(E,a)]\<sim\>a
  </equation*>

  <\proof>
    Suppose that <math|E\<sim\>a>. Then <math|R(E,a)=\<emptyset\>>, and
    <math|E\<cup\>\<emptyset\>=E>. Suppose <math|E\<nsim\>a>; we partition
    <math|F(a)> into subsets <math|S> and <math|S<rprime|'>>, where
    <math|S<rprime|'>\<subseteq\>E>. <math|F(a)-E=S>, the objects not already
    in <math|E>. Thus <math|F(a)\<subseteq\>(E\<cup\>S)>, giving
    <math|E<rprime|'>=E\<cup\>S> and <math|E<rprime|'>\<sim\>a>.
  </proof>

  We've seen how we can <em|apply> an object to an environment; by following
  the rules above, we can create environments that follow the following
  invariant: for any environment <math|E>,
  <math|(\<forall\>a\<in\>E\|E\<sim\>a)>.

  Now we turn to the task of <em|removing> an object from an environment. Let
  <math|E\<oslash\>a=E<rprime|'>> denote a new environment with <math|a>
  removed. If other objects depend on <math|a>, those objects must be
  removed, too; indeed, all objects directly or indirectly depending on
  <math|a> must be removed.

  Let <math|P(a)>, the <em|full reverse dependencies of a>, denote the set of
  objects which depend directly on <math|a>.

  <\equation*>
    P(a)=(b\<in\>E\|a\<in\>D(b))
  </equation*>

  Let <math|P<rsup|\<star\>>(a)> denote the set of objects which depend
  directly <em|or> indirectly on <math|a>.

  <\equation*>
    P<rsup|\<star\>>(a)=P(a)\<cup\><left|(><big|cup><rsub|b\<in\>P(a)>P<rsup|\<star\>>(b)<right|)>
  </equation*>

  Then to remove an object from an environment, we remove it and all of its
  reverse dependencies.

  <\equation*>
    E\<oslash\>a=E-[P<rsup|\<star\>>(a)\<cup\>{a}]
  </equation*>

  <\proof>
    Suppose that <math|P(a)=\<emptyset\>>. Then
    <math|P<rsup|\<star\>>(a)=\<emptyset\>> so <math|E\<oslash\>a=E-{a}>,
    which is the desired outcome. Suppose that
    <math|P(a)\<neq\>\<emptyset\>>.
  </proof>
</body>

<\initial>
  <\collection>
    <associate|language|american>
    <associate|page-type|letter>
    <associate|sfactor|5>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
  </collection>
</references>