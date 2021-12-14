#ifndef R
#define R 3
#endif
#ifndef N
#define N 3
#endif

// Invariant: tools in warehouse are sorted.
chan warehouse = [N] of { byte };
// Invariant: the_best_tool is equal to the value at the top of the warehouse channel
// or N + 1 if the warehouse is empty.
byte the_best_tool = N + 1;
byte peeked_tool;
// Put is nonblocking, since the number of tools in system is equal to N.
// We do not set variable x to 0, since we need it for sb_took_easier_tool.
// Both put and take preserve both invariants.
#define put(x) d_step {\
    warehouse!!x;\
    if\
    :: x < the_best_tool -> the_best_tool = x\
    :: else\
    fi\
}
#define take(x) d_step {\
    warehouse?x;\
    if\
    :: empty(warehouse) -> the_best_tool = N + 1\
    :: warehouse?<peeked_tool> -> the_best_tool = peeked_tool; peeked_tool = 0\
    fi\
}

// Worker is an adept, if he knows how to use at least one tool.
// The variable counts, how many adepts are there.
byte adepts = 0;

// Worker is a master, if he knows how to use all tools.
// The variable counts, how many masters are there.
byte masters = 0;

bool sb_changes_tool = false;
bool sb_took_easier_tool = false;

active [R] proctype worker() {
    byte breaks = N; // Worker breaks a tool if its number is less than/equal to this variable.
    byte tool = 0, tool2 = 0;
    bool change_tool = false;
    take(tool);
    do
    :: change_tool ->
           sb_changes_tool = true;
           d_step {
               if
               :: tool < tool2 -> sb_took_easier_tool = true
               :: else
               fi
               tool = tool2;
               tool2 = 0;
               change_tool = false;
               sb_changes_tool = false
           }
    :: else ->
        if
        // Worker breaks current tool.
        :: (breaks >= tool) -> d_step {
               breaks--;
               if
               :: (breaks == N - 1) -> adepts++;
               :: else
               fi;
               if
               :: (breaks == 0) -> masters++;
               :: else
               fi;
               change_tool = true;
               put(tool)
           };
           take(tool2)
        // Worker checks for new, harder tool.
        // To simulate mutex on warehouse, d_step is used.
        // The only blocking instruction in this block is the first one.
        // Take will always be executed. If the warehouse is empty, then
        // the_best_tool = N + 1 > N >= tool.
        :: d_step {
               the_best_tool < tool;
               take(tool2);
               put(tool);
               change_tool = true
           }
        fi
    od
}

init {
    // Fill the warehouse with tools (d_step, since tools should
    // already be present when workers start).
    d_step {
        byte i = N;
        do
        :: i -> put(i); i--
        :: else -> break
        od;
        skip
    }
}

#define HALT ([] (!sb_changes_tool))

// Is it true that eventually the system will halt?
ltl q1 { <> HALT }

// The question is: is it possible that the system halts before all workers
// learn all tools? Since we look for a counterexample, we ask the opposite question:
// Is it true that if the system halts then all workers are masters?
ltl q2 { [] (HALT -> masters == R) }

// Is it true that all workers will learn at least one tool?
// The answer might change depending on if N >= R or N < R.
ltl q3 { <> (adepts == R) }

// The question is: is it possible that all workers will learn all tools (if N < R)?
// Since we look for a counterexample, we ask the opposite question:
// Is it always true that not all workers will learn all tools?
ltl q4 { [] (masters < R) }

// The question is: is it possible that a worker took a tool easier than
// the one he just used? Since we look for a counterexample, we ask the opposite question:
// Is it true that workers only take harder tools (or the same one)?
ltl q5 { [] !sb_took_easier_tool }

// If the hardest tool (number 1) is available, will it be eventually taken?
ltl q6 { [] (the_best_tool == 1 -> <> (the_best_tool != 1)) }