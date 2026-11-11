import { assert, describe, it } from 'vitest';
import { declaredFramework, redrawn } from '../src/formats/declarations.js';
import { frameworkOf, parseFramework, toApx, toTgf, toTheory, type Framework } from '../src/formats/frameworks.js';

const { deepEqual, equal, throws } = assert;

const framework: Framework = { arguments: ['a', 'b', 'c'], attacks: [{ from: 'a', to: 'b' }, { from: 'b', to: 'c' }] };
const plain = ({ arguments: args, attacks }: Framework): Framework => ({ arguments: args, attacks });

describe('Framework formats', function () {
    it('should write APX and TGF', function () {
        equal(toApx(framework), 'arg(a).\narg(b).\narg(c).\natt(a,b).\natt(b,c).\n');
        equal(toTgf(framework), 'a\nb\nc\n#\na b\nb c\n');
    });

    it('should read back what it writes', function () {
        deepEqual(plain(parseFramework(toApx(framework))), framework);
        deepEqual(plain(parseFramework(toTgf(framework))), framework);
    });

    it('should read APX with spaces, comments and several facts per line', function () {
        const parsed = parseFramework('% a framework\narg( a ). arg(b).\n\natt(a , b).  % a attacks b\n');
        deepEqual(plain(parsed), { arguments: ['a', 'b'], attacks: [{ from: 'a', to: 'b' }] });
    });

    it('should read ICCMA 23, renaming the numbers to atoms', function () {
        const parsed = parseFramework('p af 3\n# a comment\n1 2\n2 3\n');
        deepEqual(plain(parsed), { arguments: ['a1', 'a2', 'a3'], attacks: [{ from: 'a1', to: 'a2' }, { from: 'a2', to: 'a3' }] });
        equal(parsed.renamed.length, 3);
    });

    it('should rename names that are not atoms, consistently and without clashes', function () {
        const parsed = parseFramework('A\nb-c\na\n#\nA b-c\n');
        deepEqual(parsed.arguments, ['a_2', 'b_c', 'a']);
        deepEqual(parsed.attacks, [{ from: 'a_2', to: 'b_c' }]);
    });

    it('should add the arguments only mentioned by attacks, and drop repeated attacks', function () {
        deepEqual(plain(parseFramework('att(a,b).\natt(a,b).\n')), { arguments: ['a', 'b'], attacks: [{ from: 'a', to: 'b' }] });
    });

    it('should explain what it cannot read', function () {
        throws(() => parseFramework('just some text'), /TGF file has a line #/);
        throws(() => parseFramework('arg(a).\nnonsense'), /Line 2/);
        throws(() => parseFramework('p af 2\n1 5\n'), /between 1 and 2/);
    });

    it('should write a framework as a theory of the profile abstract, and read it back from its graph', function () {
        equal(toTheory(framework), 'graphBuildMode(abstract).\n\na.\nb.\nc.\na -> b.\nb -> c.\n');
        const graph = {
            profile: 'abstract',
            arguments: ['a', 'b'].map((name, i) => ({ id: `A${i}`, descriptor: '', conclusion: name, topRule: name, label: 'in', supports: [] })),
            attacks: [{ from: 'A0', to: 'A1', type: 'abstract', on: 'A1' }],
        };
        deepEqual(frameworkOf(graph), { arguments: ['a', 'b'], attacks: [{ from: 'a', to: 'b' }] });
    });
});

describe('Framework declarations', function () {
    const theory = [
        'graphBuildMode(abstract).',
        '% the arguments',
        'a. arg(b).',
        'c.',
        'a -> b.',
        'att(b, c).',
        'c <-> d.',
        'X -> Y :- beats(X, Y).',
        "beats(x, y).",
        '',
    ].join('\n');

    it('should read the arguments and attacks a theory declares, in all their forms', function () {
        deepEqual(declaredFramework(theory), {
            arguments: ['a', 'b', 'c', 'd'],
            attacks: [
                { from: 'a', to: 'b' },
                { from: 'b', to: 'c' },
                { from: 'c', to: 'd' },
                { from: 'd', to: 'c' },
            ],
        });
    });

    it('should leave out clauses that are not declarations, and what is in comments or quotes', function () {
        deepEqual(declaredFramework("% a.\n/* b -> c. */\nname('x. y').\nflies(X) :- bird(X).\nz."), { arguments: ['z'], attacks: [] });
    });

    it('should read no argument in the switches of the flags, and leave them as they are', function () {
        const switched = 'graphBuildMode(abstract).\nqueryMode.\na.\nmetaConflicts.\narg(metaRules).\nb -> a.\n';
        deepEqual(declaredFramework(switched), { arguments: ['a', 'metaRules', 'b'], attacks: [{ from: 'b', to: 'a' }] });
        equal(redrawn(switched, { arguments: ['a'], attacks: [] }), 'graphBuildMode(abstract).\nqueryMode.\na.\nmetaConflicts.\n');
    });

    it('should leave a theory drawn as it is as it is', function () {
        equal(redrawn(theory, declaredFramework(theory)), theory);
    });

    it('should add what is drawn after the last declaration', function () {
        const drawn = declaredFramework(theory);
        const added = redrawn(theory, { arguments: [...drawn.arguments, 'e'], attacks: [...drawn.attacks, { from: 'e', to: 'a' }] });
        equal(added, theory.replace('c <-> d.', 'c <-> d.\ne.\ne -> a.'));
    });

    it('should remove what is no longer drawn, with its line when nothing else is on it', function () {
        const drawn = declaredFramework(theory);
        const removed = redrawn(theory, {
            arguments: drawn.arguments.filter(it => it !== 'b'),
            attacks: drawn.attacks.filter(it => it.from !== 'b' && it.to !== 'b'),
        });
        equal(removed, theory.replace('a. arg(b).', 'a. ').replace('a -> b.\natt(b, c).\n', ''));
    });

    it('should leave half of a mutual attack as a plain one', function () {
        const drawn = declaredFramework(theory);
        const half = redrawn(theory, { arguments: drawn.arguments, attacks: drawn.attacks.filter(it => it.from !== 'c' || it.to !== 'd') });
        equal(half, theory.replace('c <-> d.', 'd -> c.'));
    });

    it('should rename an argument where it is declared, in the form it is written in', function () {
        const drawn = declaredFramework(theory);
        const rename = (it: string) => (it === 'b' ? 'bee' : it);
        const renamed = redrawn(
            theory,
            { arguments: drawn.arguments.map(rename), attacks: drawn.attacks.map(it => ({ from: rename(it.from), to: rename(it.to) })) },
            new Map([['b', 'bee']]),
        );
        equal(renamed, theory.replace('arg(b)', 'arg(bee)').replace('a -> b.', 'a -> bee.').replace('att(b, c)', 'att(bee, c)'));
    });

    it('should start a theory with the profile when it was empty', function () {
        equal(redrawn('', { arguments: ['a', 'b'], attacks: [{ from: 'a', to: 'b' }] }), 'graphBuildMode(abstract).\n\na.\nb.\na -> b.\n');
        equal(redrawn('% mine', { arguments: ['a'], attacks: [] }), '% mine\na.\n');
    });
});
