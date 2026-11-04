# Notes for agents

## Ideas, not yet done

### hsgui: matching back buttons for Step and Tick

The toolbar has Step, Tick and a single Step back, which undoes whichever
came last, a step or a tick. It might be clearer to have two matching pairs:

- Step back / Step: whole steps (all six tick phases).
- Tick back / Tick: one phase at a time.

Step back would then undo back to the start of the round (the Snow phase),
even if the last action was a tick. The undo history is in `hscanvas.ml`
(`save`, `step_back`), and each saved entry already records the tick phase.
