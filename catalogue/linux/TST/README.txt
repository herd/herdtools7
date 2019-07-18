Run a model as herd7 -conf loc.cfg -model <name>.cat

Experimental models in directory : models/

The most recent model is L01.cat that follows the original linux.cat model,
except that :

  *  grace period and read-side critical sections, which are new.
  * Annotation Ctrl is ignored in model (ie control dependencies to W
    are considered as being dependencies, whithout teh original R to
    be annotated.

