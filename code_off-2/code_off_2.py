"""
This code is a Python translation of the Haskell solution (see CodeOff2.hs).

Rather than translate only the implementation code, I decided to experiment and
see to what extent PEP 484 type hints and the MyPy type checker can represent
and verify the same type information as the Haskell implementation.

To install and run the MyPy type checker:

    pip install mypy-lang
    mypy code_off_2.py

:author: Piët Delport <pjdelport@gmail.com>
"""
import sys
from operator import itemgetter
from typing import *


# Type synonyms for easier reading.
Litre = int
LiquidType = int
Reservoir = Dict[LiquidType, Litre]

# A jar and its compatible liquid types.
Jar = NamedTuple('Jar', [('capacity', Litre), ('compatible_types',  List[LiquidType])])

# A jar filled with some liquid type.
FilledJar = NamedTuple('FilledJar', [('amount', Litre), ('type', LiquidType)])


def parse_jar(s: str) -> Jar:
    """
    Parse a jar specification. For example:

    >>> parse_jar('100,0,1')
    Jar(capacity=100, compatible_types=[0, 1])
    """
    try:
        liters, *types = map(int, s.split(','))
    except ValueError:
        raise ValueError('parse_jar: bad spec: {!r}'.format(s))
    else:
        return Jar(liters, list(types))


def drain(amount: Litre, liquid: LiquidType, res: Reservoir) -> Reservoir:
    """
    Helper: Remove liquid from a reservoir. This returns a copy.
    """
    res = res.copy()
    if liquid in res:
        res[liquid] -= amount
        if res[liquid] == 0:
            del res[liquid]
        elif res[liquid] < 0:
            raise AssertionError('drain: negative remainder!')
    return res


def fill_jar(res: Reservoir, jar: Jar) -> Iterable[Tuple[Reservoir, FilledJar]]:
    """
    Fill a jar with each compatible liquid type.

    This returns a list of filled jars and drained reservoirs, one for each
    compatible liquid type. For example:

    >>> list(fill_jar({0: 10, 1: 4}, Jar(5, [0,1,2]))
    [({0: 5, 1: 4},  FilledJar(amount=5, type=0)),
     ({0: 10},       FilledJar(amount=4, type=1)),
     ({0: 10, 1: 4}, FilledJar(amount=0, type=2))]
    """
    for t in jar.compatible_types:
        amount = min(jar.capacity, res.get(t, 0))
        yield (drain(amount, t, res), FilledJar(amount, t))


def fill_jars(res: Reservoir, jars: List[Jar]) -> Iterable[Tuple[Reservoir, List[FilledJar]]]:
    """
    Fill a list of jars in sequence. This combines the lists of alternative
    fillings for each individual jar into a list of alternative fillings for
    the whole input list of jars.

    (This uses map_accum_iter(), a Python version of Haskell's mapAccumM,
    specialised to lists; see below.)
    """
    return map_accum_iter(fill_jar, res, jars)


# The following probably requires some explanation.
#
# In Haskell, mapAccumM is the monadic version of mapAccumL, and may be defined
# as follows::
#
#     mapAccumM :: Monad m
#               => (acc -> x -> m (acc, y))
#               -> acc
#               -> [x]
#               -> m (acc, [y])
#     mapAccumM _ s [] = return (s, [])
#     mapAccumM f s (x:xs) = do
#         (s', y) <- f s x
#         (s'', ys) <- mapAccumM f s' xs
#         return (s'', y:ys)
#
# Base Python does not have a trivial way to introduce the Monad type class,
# but since we only use the list instance of it, we can express the
# list-specialized version of it in Python, complete with all the types.
#
# Note that the closest analogue of Haskell's [] type in Python is not List,
# but Iterable: this is what we constrain the Haskell signature's m to.

M = Iterable            # The choice of monad, constrained to Iterable (Haskell's []).
Acc = TypeVar('Acc')    # Accumulator
X = TypeVar('X')        # Input elements
Y = TypeVar('Y')        # Output elements
def map_accum_iter(f: Callable[[Acc, X], M[Tuple[Acc, Y]]],
                   acc: Acc,
                   l: List[X],
                   ) -> M[Tuple[Acc, List[Y]]]:
    if not l:
        # MyPy (0.3) needs the cast for an empty list literal.
        yield (acc, cast(List[Y], []))
    else:
        (x, xs) = (l[0], l[1:])
        for (acc2, y) in f(acc, x):
            for (acc3, ys) in map_accum_iter(f, acc2, xs):
                yield (acc3, [y] + ys)


def readlines(f: 'TextIO', n: int) -> Iterable[str]:
    for _ in range(n):
        yield f.readline().rstrip('\n')


def main() -> None:
    # Read the available liquids.
    num_liquids = int(sys.stdin.readline())
    initial_reservoir = {i: int(line) for (i, line) in
                         enumerate(readlines(sys.stdin, num_liquids))}

    # Read the input jars.
    num_jars = int(sys.stdin.readline())
    jars = [parse_jar(line) for line in readlines(sys.stdin, num_jars)]

    # Get a solution with minimum remaining liquid.
    #
    # Note: The following intentionally keeps the solutions iterable lazy, so
    # that min() can find the minimum without keeping all the solutions in
    # memory at once.
    solutions = fill_jars(initial_reservoir, jars)
    scored = ((sum(res.values()), js) for (res, js) in solutions)
    # TODO: Assert we at least one solution.
    (remainder, filled_jars) = min(scored, key=itemgetter(0))

    # Format the output.
    print(remainder)
    for (amount, liquid) in filled_jars:
        print('{},{}'.format(liquid, amount))


if __name__ == '__main__':
    main()
