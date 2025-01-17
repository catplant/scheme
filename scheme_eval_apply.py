import sys

from pair import *
from scheme_utils import *
from ucb import main, trace

import scheme_forms

##############
# Eval/Apply #
##############

def scheme_eval(expr, env, _=None): # Optional third argument is ignored
    """Evaluate Scheme expression EXPR in Frame ENV.

    >>> expr = read_line('(+ 2 2)')
    >>> expr
    Pair('+', Pair(2, Pair(2, nil)))
    >>> scheme_eval(expr, create_global_frame())
    4
    """
    # Evaluate atoms
    if scheme_symbolp(expr):
        return env.lookup(expr)
    elif self_evaluating(expr):
        return expr

    # All non-atomic expressions are lists (combinations)
    if not scheme_listp(expr):
        raise SchemeError('malformed list: {0}'.format(repl_str(expr)))
    first, rest = expr.first, expr.rest
    if scheme_symbolp(first) and first in scheme_forms.SPECIAL_FORMS:
        return scheme_forms.SPECIAL_FORMS[first](rest, env)
    else:
        # BEGIN PROBLEM 3
        "*** YOUR CODE HERE ***"
        #print("DEBUG: Called: scheme_eval")
        """
        You'll have to recursively call scheme_eval in the first two steps. 
        Here are some other functions/methods you should use:

        -The map method of Pair returns a new Scheme list constructed by 
        applying a one-argument function to every item in a Scheme list. 
        Think about what function we want to apply to every operand.

        -The scheme_apply function applies a Scheme procedure to arguments 
        represented as a Scheme list (a Pair instance or nil).
        """
        # Step 1: Evaluate the operator (which should evaluate to a Procedure instance. 
        # See scheme_classes.py for Procedure definitions).
        operator = expr.first
        operator_evaluated = scheme_eval(operator, env)

        # Step 2: Evaluate all of the operands and collect the results (the argument values) in a Scheme list.
        operands = expr.rest
        # Pair.map(self, fn)
        collect_list_scm = operands.map(lambda x: scheme_eval(x, env))

        # Step 3: Return the result of calling scheme_apply on this Procedure and these argument values.
        return scheme_apply(operator_evaluated, collect_list_scm, env)
        
        # END PROBLEM 3

def scheme_apply(procedure, args, env):
    """Apply Scheme PROCEDURE to argument values ARGS (a Scheme list) in
    Frame ENV, the current environment."""
    validate_procedure(procedure)
    if not isinstance(env, Frame):
       assert False, "Not a Frame: {}".format(env)
    if isinstance(procedure, BuiltinProcedure):
        # BEGIN PROBLEM 2
        "*** YOUR CODE HERE ***"
        #print("DEBUG: Called: scheme_apply")
        """
        Convert the Scheme list to a Python list of arguments. 
        Hint: args is a Pair, which has .first and .rest attributes.

        If procedure.need_env is True, then add the current environment 
        env as the last argument to this Python list.
        """
        # convert scheme list into a python list of arguments
        py_list = []
        while args is not nil:
            py_list.append(args.first)
            args = args.rest

        # bool check of env
        if procedure.need_env:
            py_list.append(env)

        # END PROBLEM 2
        try:
            # BEGIN PROBLEM 2
            "*** YOUR CODE HERE ***"
            return procedure.py_func(*py_list)
            # END PROBLEM 2
        except TypeError as err:
            raise SchemeError('incorrect number of arguments: {0}'.format(procedure))
    elif isinstance(procedure, LambdaProcedure):
        # BEGIN PROBLEM 9
        #print("DEBUG: Called scheme_apply (LambdaProcedure)")
        """
        First create a new Frame instance and bind the procedure's formal parameters 
        to the argument values by calling the make_child_frame method on the appropriate
        parent frame.
        """
        #print("DEBUG: procedure = ", procedure) # (lambda (x) (* x x))
        #print("DEBUG: arguments = ", args) # (21)
        #print("DEBUG: environment = ", env) # <Global Frame>

        """
        lexical scoping: the parent of the new call frame is the environment in which the procedure was defined
        """
        frem = procedure.env
        new_frame_instance = frem.make_child_frame(procedure.formals, args) # make_child_frame(formals, vals)
        #print("DEBUG: new frame instance =", new_frame_instance)
        
        """
        Second, within this new frame, evaluate each of the expressions of the body of 
        the procedure using eval_all. eval_all(expressions, environment).
        """
        return eval_all(procedure.body, new_frame_instance)

        # END PROBLEM 9
    elif isinstance(procedure, MuProcedure):
        # BEGIN PROBLEM 11
        """
        dynamic scoping: the parent of the new call frame is the environment in which the call expression was evaluated.

        When a MuProcedure is called, 
        the parent of the new call frame is the environment in which that call expression was evaluated. 
        As a result, a MuProcedure does not need to store an environment as an instance attribute. 
        """
        return eval_all(procedure.body, env.make_child_frame(procedure.formals, args))
        # END PROBLEM 11
    else:
        assert False, "Unexpected procedure: {}".format(procedure)

def eval_all(expressions, env):
    """Evaluate each expression in the Scheme list EXPRESSIONS in
    Frame ENV (the current environment) and return the value of the last.

    >>> eval_all(read_line("(1)"), create_global_frame())
    1
    >>> eval_all(read_line("(1 2)"), create_global_frame())
    2
    >>> x = eval_all(read_line("((print 1) 2)"), create_global_frame())
    1
    >>> x
    2
    >>> eval_all(read_line("((define x 2) x)"), create_global_frame())
    2
    """
    # BEGIN PROBLEM 6
    #print("DEBUG: Called: eval_all")
    if expressions == nil:
        return None
    else:
        container = []
        while expressions:
            container.append(scheme_eval(expressions.first, env))
            expressions = expressions.rest
        return container[-1]
    
    # END PROBLEM 6


################################
# Extra Credit: Tail Recursion #
################################

class Unevaluated:
    """An expression and an environment in which it is to be evaluated."""

    def __init__(self, expr, env):
        """Expression EXPR to be evaluated in Frame ENV."""
        self.expr = expr
        self.env = env

def complete_apply(procedure, args, env):
    """Apply procedure to args in env; ensure the result is not an Unevaluated."""
    validate_procedure(procedure)
    val = scheme_apply(procedure, args, env)
    if isinstance(val, Unevaluated):
        return scheme_eval(val.expr, val.env)
    else:
        return val

def optimize_tail_calls(unoptimized_scheme_eval):
    """Return a properly tail recursive version of an eval function."""
    def optimized_eval(expr, env, tail=False):
        """Evaluate Scheme expression EXPR in Frame ENV. If TAIL,
        return an Unevaluated containing an expression for further evaluation.
        """
        if tail and not scheme_symbolp(expr) and not self_evaluating(expr):
            return Unevaluated(expr, env)

        result = Unevaluated(expr, env)
        # BEGIN OPTIONAL PROBLEM 1
        "*** YOUR CODE HERE ***"
        # END OPTIONAL PROBLEM 1
    return optimized_eval














################################################################
# Uncomment the following line to apply tail call optimization #
################################################################

# scheme_eval = optimize_tail_calls(scheme_eval)
