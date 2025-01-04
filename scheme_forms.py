from scheme_eval_apply import *
from scheme_utils import *
from scheme_classes import *
from scheme_builtins import *

#################
# Special Forms #
#################

# Each of the following do_xxx_form functions takes the cdr of a special form as
# its first argument---a Scheme list representing a special form without the
# initial identifying symbol (if, lambda, quote, ...). Its second argument is
# the environment in which the form is to be evaluated.

def do_define_form(expressions, env):
    """Evaluate a define form.
    >>> env = create_global_frame()
    >>> do_define_form(read_line("(x 2)"), env) # evaluating (define x 2)
    'x'
    >>> scheme_eval("x", env)
    2
    >>> do_define_form(read_line("(x (+ 2 8))"), env) # evaluating (define x (+ 2 8))
    'x'
    >>> scheme_eval("x", env)
    10
    >>> # problem 10
    >>> env = create_global_frame()
    >>> do_define_form(read_line("((f x) (+ x 2))"), env) # evaluating (define (f x) (+ x 8))
    'f'
    >>> scheme_eval(read_line("(f 3)"), env)
    5
    """
    validate_form(expressions, 2) # Checks that expressions is a list of length at least 2
    signature = expressions.first
    if scheme_symbolp(signature):
        # assigning a name to a value e.g. (define x (+ 1 2))
        validate_form(expressions, 2, 2) # Checks that expressions is a list of length exactly 2
        # BEGIN PROBLEM 4
        "*** YOUR CODE HERE ***"
        #print("DEBUG: Called: scheme_symbolp")
        # when operand is symbol
        #print("DEBUG: DO DEFINE FORM, env=", env)

        first_operand = expressions.first
        #print("DEBUG: 1st operand = ", first_operand, type(first_operand))
        
        second_operand = expressions.rest.first
        #print("DEBUG: 2nd operand = ", second_operand, type(second_operand))
        second_operand_evaluated = scheme_eval(second_operand, env)
        #print("DEBUG: evaled 2nd op = ", second_operand_evaluated, type(second_operand_evaluated))

        # env.define(self, symbol, val)
        env.define(first_operand, second_operand_evaluated)

        #print("DEBUG: returned 1st operand = ", first_operand, type(first_operand))
        return first_operand
    
        # END PROBLEM 4

    elif isinstance(signature, Pair) and scheme_symbolp(signature.first):
        # defining a named procedure e.g. (define (f x y) (+ x y))
        # BEGIN PROBLEM 10
        #print("DEBUG: Called: do_define_form (Pair)")
        """
        (define (f x) (* x 2))
        --> (define f (lambda (x) (* x 2)))
        """
        e = expressions # e = ((f x y) (+ x y)) <class 'pair.Pair'>
        #print("DEBUG: expression =", e, type(e)) 
        s = signature # s =  (f x y) <class 'pair.Pair'>
        #print("DEBUG: s = ", s, type(s))

        s_name = s.first # name =  f <class 'str'>
        #print("DEBUG: name = ", s_name, type(s_name))
        s_args = s.rest # arguments =  (x y) <class 'pair.Pair'>
        #print("DEBUG: arguments = ", s_args, type(s_args))

        # check if arguments are literals, because otherwise function will be bricked
        temp = s_args
        while temp:
            if type(temp.first) is not str:
                raise SchemeError
            temp = temp.rest

        e_body = e.rest # body =  (+ x y) <class 'pair.Pair'>
        #print("DEBUG: body = ", e_body, type(e_body))

        """
        Construct an expression (define _ (lambda ...)) and call 
        do_define_form on it (omitting the define). 
        """
        lambda_and_beyond = LambdaProcedure(s_args, e_body, env)
        #: lam & bey = ", lambda_and_beyond, type(lambda_and_beyond))

        env.define(s_name, lambda_and_beyond)

        #print("DEBUG: returned s_name = ", s_name, type(s_name))
        return s_name
        # END PROBLEM 10
    else:
        bad_signature = signature.first if isinstance(signature, Pair) else signature
        raise SchemeError('non-symbol: {0}'.format(bad_signature))

def do_quote_form(expressions, env):
    """Evaluate a quote form.

    >>> env = create_global_frame()
    >>> do_quote_form(read_line("((+ x 2))"), env) # evaluating (quote (+ x 2))
    Pair('+', Pair('x', Pair(2, nil)))
    """
    validate_form(expressions, 1, 1)
    # BEGIN PROBLEM 5
    "*** YOUR CODE HERE ***"
    #print("DEBUG: Called: do_quote_form")
    return expressions.first
    # END PROBLEM 5

def do_begin_form(expressions, env):
    """Evaluate a begin form.

    >>> env = create_global_frame()
    >>> x = do_begin_form(read_line("((print 2) 3)"), env) # evaluating (begin (print 2) 3)
    2
    >>> x
    3
    """
    validate_form(expressions, 1)
    return eval_all(expressions, env)

def do_lambda_form(expressions, env):
    """Evaluate a lambda form.

    >>> env = create_global_frame()
    >>> do_lambda_form(read_line("((x) (+ x 2))"), env) # evaluating (lambda (x) (+ x 2))
    LambdaProcedure(Pair('x', nil), Pair(Pair('+', Pair('x', Pair(2, nil))), nil), <Global Frame>)
    """
    validate_form(expressions, 2)
    formals = expressions.first
    validate_formals(formals)
    # BEGIN PROBLEM 7
    "*** YOUR CODE HERE ***"
    #print("DEBUG: Called: do_lambda_form")
    # creates and returns a lambda procedure instance
    # body (expressions.rest.rest) must contain at least 1 expression
    body = expressions.rest
    return LambdaProcedure(formals, body, env)

    # END PROBLEM 7

def do_if_form(expressions, env):
    """Evaluate an if form.

    >>> env = create_global_frame()
    >>> do_if_form(read_line("(#t (print 2) (print 3))"), env) # evaluating (if #t (print 2) (print 3))
    2
    >>> do_if_form(read_line("(#f (print 2) (print 3))"), env) # evaluating (if #f (print 2) (print 3))
    3
    """
    validate_form(expressions, 2, 3)
    if is_scheme_true(scheme_eval(expressions.first, env)):
        return scheme_eval(expressions.rest.first, env)
    elif len(expressions) == 3:
        return scheme_eval(expressions.rest.rest.first, env)

def do_and_form(expressions, env):
    """Evaluate a (short-circuited) and form.

    >>> env = create_global_frame()
    >>> do_and_form(read_line("(#f (print 1))"), env) # evaluating (and #f (print 1))
    False
    >>> # evaluating (and (print 1) (print 2) (print 4) 3 #f)
    >>> do_and_form(read_line("((print 1) (print 2) (print 3) (print 4) 3 #f)"), env)
    1
    2
    3
    4
    False
    """
    # BEGIN PROBLEM 12
    "*** YOUR CODE HERE ***"
    #print("DEBUG: Called: do and form")

    if expressions is nil:
        return True

    result = True
    while expressions:
        curr = scheme_eval(expressions.first, env)

        if is_scheme_false(curr):
            return False
        
        result = curr
        expressions = expressions.rest
    
    return result
            
    
    # END PROBLEM 12

def do_or_form(expressions, env):
    """Evaluate a (short-circuited) or form.

    >>> env = create_global_frame()
    >>> do_or_form(read_line("(10 (print 1))"), env) # evaluating (or 10 (print 1))
    10
    >>> do_or_form(read_line("(#f 2 3 #t #f)"), env) # evaluating (or #f 2 3 #t #f)
    2
    >>> # evaluating (or (begin (print 1) #f) (begin (print 2) #f) 6 (begin (print 3) 7))
    >>> do_or_form(read_line("((begin (print 1) #f) (begin (print 2) #f) 6 (begin (print 3) 7))"), env)
    1
    2
    6
    """
    # BEGIN PROBLEM 12
    "*** YOUR CODE HERE ***"
    #print("DEBUG: Called: do or form")

    if expressions is nil:
        return False

    result = True
    while expressions:
        curr = scheme_eval(expressions.first, env)

        if is_scheme_true(curr):
            return curr

        result = curr
        expressions = expressions.rest
    
    return result

    # END PROBLEM 12

def do_cond_form(expressions, env):
    """Evaluate a cond form.

    >>> do_cond_form(read_line("((#f (print 2)) (#t 3))"), create_global_frame())
    3
    """
    while expressions is not nil:
        clause = expressions.first
        validate_form(clause, 1)
        if clause.first == 'else':
            test = True
            if expressions.rest != nil:
                raise SchemeError('else must be last')
        else:
            test = scheme_eval(clause.first, env)
        if is_scheme_true(test):
            # BEGIN PROBLEM 13
            #print("DEBUG: Called: do cond form")
            #print("DEBUG: Do Cond Form: Expressions =", expressions)
            #print("DEBUG: Do Cond Form: Clause =", clause)
            #print("DEBUG: Do Cond Form: Clause First =", clause.first)
            #print("DEBUG: Do Cond Form: Clause Rest =", clause.rest)


            # Return the value of the first result sub-expression corresponding to a true predicate,
            # - *When the true predicate does not have a corresponding result sub-expression, return the predicate value.
            # - **When a result sub-expression of a cond case has multiple expressions, evaluate them all and return 
            #   the value of the last expression. (Hint: Use eval_all.)

            # Or return the value of the resulting sub-expression corresponding to else.
            # If there is only an else, return the value of its result sub-expression. 
            # *If it doesn't have one, return #t.

            # The value of a cond is undefined if there are no true predicates and no else => Return None
            if clause.rest is not nil:
                if clause.first == "else":
                    return clause.rest.first
                elif clause.rest.rest is not nil:
                    sub_expr = eval_all(clause.rest, env)
                    #print("DEBUG: sub_expr,", sub_expr)
                    return sub_expr
                else:
                    sub_expr = scheme_eval(clause.rest.first, env)
                    #print("DEBUG: sub_expr,", sub_expr)
                    return sub_expr
                
            elif clause.rest is nil:
                if clause.first == "else":
                    return True
                else:
                    # return test here because predicate has already been evaluated when checking w/ test name
                    predicate = test
                    #print("DEBUG: predicate,", predicate)
                    return predicate
            else:
                return None

            
            # END PROBLEM 13
        expressions = expressions.rest

def do_let_form(expressions, env):
    """Evaluate a let form.

    >>> env = create_global_frame()
    >>> do_let_form(read_line("(((x 2) (y 3)) (+ x y))"), env)
    5
    """
    validate_form(expressions, 2)
    let_env = make_let_frame(expressions.first, env)
    return eval_all(expressions.rest, let_env)

def make_let_frame(bindings, env):
    """Create a child frame of Frame ENV that contains the definitions given in
    BINDINGS. The Scheme list BINDINGS must have the form of a proper bindings
    list in a let expression: each item must be a list containing a symbol
    and a Scheme expression."""
    if not scheme_listp(bindings):
        raise SchemeError('bad bindings list in let form')
    names = vals = nil
    # BEGIN PROBLEM 14
    #print("DEBUG: Called: make_let_frame")
    """
    # bindings cannot have 2 vals for 1 key
    # binding keys must be unique, no repeats

    validate_form(bindings.first, 2) # this function can be used to validate the structure of each binding.
    validate_formals(bindings) # this function validates that its argument is a Scheme list of symbols for which each symbol is distinct.
    """

    while bindings:
        #print("DEBUG: Bindings:", bindings)

        lst = bindings.first
        frml = bindings.first.first

        # val must exist
        if bindings.first.rest is nil:
            raise SchemeError
        # bindings cannot have more than 1 val
        if bindings.first.rest.rest is not nil:
            raise SchemeError

        val = bindings.first.rest.first

        if type(val) is not int:
            val = scheme_eval(val, env)

        #print("DEBUG: Lst / Frml / Val --", lst, "/", frml, "/", val)
        #print("DEBUG: Lst / Frml / Val --", type(lst), "/", type(frml), "/", type(val))

        names = Pair(frml, names)
        vals = Pair(val, vals)
        bindings = bindings.rest
    #print("DEBUG: Names, Vals =", names, vals)
    # this function validates that its argument is a Scheme list of symbols for which each symbol is distinct.
    validate_formals(names)

    # END PROBLEM 14
    return env.make_child_frame(names, vals) # formals, vals



def do_quasiquote_form(expressions, env):
    """Evaluate a quasiquote form with parameters EXPRESSIONS in
    Frame ENV."""
    def quasiquote_item(val, env, level):
        """Evaluate Scheme expression VAL that is nested at depth LEVEL in
        a quasiquote form in Frame ENV."""
        if not scheme_pairp(val):
            return val
        if val.first == 'unquote':
            level -= 1
            if level == 0:
                expressions = val.rest
                validate_form(expressions, 1, 1)
                return scheme_eval(expressions.first, env)
        elif val.first == 'quasiquote':
            level += 1

        return val.map(lambda elem: quasiquote_item(elem, env, level))

    validate_form(expressions, 1, 1)
    return quasiquote_item(expressions.first, env, 1)

def do_unquote(expressions, env):
    raise SchemeError('unquote outside of quasiquote')


#################
# Dynamic Scope #
#################

def do_mu_form(expressions, env):
    """Evaluate a mu form."""
    validate_form(expressions, 2)
    formals = expressions.first
    validate_formals(formals)
    # BEGIN PROBLEM 11
    "*** YOUR CODE HERE ***"
    #print("DEBUG: Called: do_mu_form")

    """
    Implement do_mu_form in scheme_forms.py to evaluate the mu special form. 
    A mu expression evaluates to a MuProcedure. 
    The MuProcedure class (defined in scheme_classes.py) has been provided for you.
    """

    # A mu expression evaluates to a MuProcedure
    # MuProcedure(formals, body)
    expressions_formals = expressions.first
    expressions_body = expressions.rest
    mu_form = MuProcedure(expressions_formals, expressions_body)

    return mu_form
    # END PROBLEM 11



SPECIAL_FORMS = {
    'and': do_and_form,
    'begin': do_begin_form,
    'cond': do_cond_form,
    'define': do_define_form,
    'if': do_if_form,
    'lambda': do_lambda_form,
    'let': do_let_form,
    'or': do_or_form,
    'quote': do_quote_form,
    'quasiquote': do_quasiquote_form,
    'unquote': do_unquote,
    'mu': do_mu_form,
}