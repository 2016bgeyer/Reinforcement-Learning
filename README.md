# Reinforcement-Learning
Q Learning For the game Nim

This should not be particularly difficult, and you could probably get it done this week if you wished.
A Q-learner that learns to play Nim.

This project is an easy assignment.
What I'm asking you to do is to complete the assignment, then
"enhance" it in some way.  Try three heaps; try prioritorized sweeping
(ask me); try comparing approaches to alpha, gamma, action-selection procedures,
ways of implementing the "opponent, etc.  Try extending to 3-Heap Nim.
Something fun.

You might also try to analyze what happens when you try random opponents versus co-adaptive ones.  Advantages?  Disadvantages?

ABOUT NIM
---------
There are several versions of Nim.  The version we will play is called 1-Heap Nim
and it goes like this:

1. Put N sticks in a pile (called a "heap")
2. Players take turns taking 1, 2, or 3 sticks out of the heap.
3. Whoever has to take out the last stick loses.


LEARNING NIM
------------

Our Q-learner will build a Q-table solely through playing itself over and over
again.  The table will tell it, ultimately, how smart it is to do a given move
(take 1, 2, or 3 sticks) in a given state (number of sticks taken out so far).
Q values will all start at 0.

We will define the actions as follows:

Action 0: take 1 stick out
Action 1: take 2 sticks out
Action 2: take 3 sticks out

Thus the action number is exactly 1- the number of sticks to take out.  Keep
this in mind -- the Q table will store Q values by action number, NOT by
sticks taken out.

We will define the states as follows:

State 0: no sticks removed from heap
State 1: 1 stick removed from heap
...
State N: N sticks removed from heap

You will probably find it useful for the number of states in the Q table to
be, believe it or not, about 6 larger than the heap size.  Thus there are
some states at the high end of the table which represent, more or less,
"negative heap sizes".  Of course, you can never play a negative heap size;
such q-values will stay 0.

Our Q table will be a STATE x ACTION array.  I have given you some functions
which should make it easy to use this array:  NUM-STATES, NUM-ACTIONS,
MAKE-Q-TABLE, MAX-Q, and MAX-ACTION.

The Q learner will learn by playing itself: the learner records the current
state, makes a move, lets the opponent make a move, then notes the new
resulting state.  The action is the move the learner made.  Now we have s,
a, and s'.  Note that s' is the state AFTER the opponent made his move.

After the Q learner has learned the game, then you can play the learner
and see how well it does.


WHAT YOU NEED TO DO
-------------------

Your job is to implement several functions:

Q-LEARNER
  (the Q update function)
LEARN-NIM
  (the learning algorithm, tweaked for Nim -- the longest function)
PLAY-NIM
  (lets you play against the learned Q table)
BEST-ACTIONS
  (prints out the best actions believed so far)

To help you, I've written a basic ALPHA function, and MAKE-USER-MOVE
and ASK-IF-USER-GOES-FIRST functions.  I predict you will find them helpful.



THE SECRET OF NIM (ugh, that was bad)
-----------------

You can get an idea for how well these settings perform by seeing what's
usually the smallest number of iterations necessary before BEST-ACTIONS starts
reporting the correct actions.

So what ARE the correct actions in Nim?  There is a very simple rule for playing
Nim.  If there are N sticks left in the pile, you want to remove sticks so that
N = 1 + 4A where A is some number.  Then whatever your opponent takes out, you take
4 minus that number, so your sticks and your opponent's sticks removed sum to 4.
Keep on doing this, and eventually the A's will get dropped and your opponent will
be left with 1 stick, which he must take.

Depending on the size of the Nim heap, the game is either a guaranteed win for
the first player or for the second player.  It all depends on who can get it down
to 1 + 4A first.

You will discover a certain pattern emerge in your BEST-ACTIONS list.  The first
couple of values may be odd, but then from there on out you'll see
2, 1, 0, <any>, 2, 1, 0, <any>, etc.  This is because in each of those heap
values, the right move is to remove 3, 2, or 1 sticks, or (in the <any> value)
it doesn't matter because you're guaranteed to lose at that heap size.  In essence
you want to get your OPPONENT down to the <any> value (it's the 1 + 4A number).


VERY STRONG HINT

Keep in mind how the Q table is structured: actions are stored in the slot
1 less than the number of sticks removed by that action.  And states go UP
as more sticks are removed.   You may need to do some 1-'s and 1+'s to play
the right action.


INTERESTING TRIVIA

Nim's been done a lot.  I was going to do tic-tac-toe, but decided it was too
evil.  :-)
