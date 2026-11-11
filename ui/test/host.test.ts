import { assert, describe, it } from 'vitest';
import * as api from '@tuprolog/arg2p';
import { DEFAULT_FLAGS, FLAGS as SCHEMA, PROFILES } from '../src/flags/schema.js';
import { check, createHost, settings, type Answer, type Request } from '../src/js/host.js';
import { JsEngine, type Channel } from '../src/js/engine.js';

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

const FLAGS = 'statementLabellingMode(statement).\nargumentLabellingMode(complete).\n';
const TWEETY = 'f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).';

// A host whose answers are collected, as the page would receive them
function host() {
    const posted: Answer[] = [];
    const { handle } = createHost(answer => posted.push(JSON.parse(JSON.stringify(answer)) as Answer));
    const send = <T extends Answer['type']>(request: Request): Extract<Answer, { type: T }> => {
        handle(request);
        return posted[posted.length - 1] as Extract<Answer, { type: T }>;
    };
    return { posted, send };
}

// The engine of the browser on a host in the same thread, answering as a worker would (later); the channels it
// opened, last the one in use
function engine(channels: Channel[] = []): JsEngine {
    return new JsEngine(() => {
        const channel: Channel = {
            onmessage: null,
            onerror: null,
            postMessage: request => setTimeout(() => handle(request), 0),
            terminate: () => {},
        };
        const { handle } = createHost(answer => channel.onmessage?.({ data: answer }));
        channels.push(channel);
        return channel;
    });
}

const failure = (promise: Promise<unknown>) =>
    promise.then(
        () => null,
        (it: unknown) => it as { name: string; stopped?: boolean },
    );

describe('Host of the engine', function () {
    it('should warm up', function () {
        equal(host().send({ type: 'warmUp' }).type, 'warm');
    });

    it('should give the solutions one at a time, with their graphs, as plain data', function () {
        const { send } = host();
        deepEqual(send({ type: 'solve', id: 1, theory: 'r1 :=> a.\nr2 :=> -a.\n', query: 'solve(In, Out, Und)', flags: FLAGS }), {
            type: 'started',
            id: 1,
            query: 'solve(In, Out, Und)',
        });
        const labellings: string[] = [];
        let answer: Extract<Answer, { type: 'solution' }>;
        do {
            answer = send({ type: 'next', id: 1 });
            equal(answer.type, 'solution');
            if (answer.solution.result === 'yes') labellings.push(answer.solution.graph.arguments.map(it => it.label).join());
        } while (answer.hasNext);
        equal(labellings.length, 3);
        equal(new Set(labellings).size, 3);
    });

    it('should give the structure of the arguments, and the profile they were built in', function () {
        const { send } = host();
        send({ type: 'solve', id: 2, theory: TWEETY, query: 'solve', flags: FLAGS });
        const { graph } = send<'solution'>({ type: 'next', id: 2 }).solution;
        equal(graph.profile, 'aspicPlus');
        const flies = graph.arguments.find(it => it.conclusion === 'flies(tweety)')!;
        equal(flies.topRule, 'r1');
        deepEqual(flies.supports.map(id => graph.arguments.find(it => it.id === id)!.conclusion), ['bird(tweety)']);
        ok(graph.attacks.some(it => it.to === flies.id && it.type === 'rebut'));
    });

    it('should read an abstract framework from its theory', function () {
        const { send } = host();
        send({ type: 'solve', id: 3, theory: 'graphBuildMode(abstract).\na.\nb.\nc.\na -> b.\nb <-> c.', query: 'solve(In, Out, Und)', flags: '' });
        const { graph } = send<'solution'>({ type: 'next', id: 3 }).solution;
        equal(graph.profile, 'abstract');
        deepEqual(graph.arguments.map(it => `${it.conclusion}:${it.label}`).sort(), ['a:in', 'b:out', 'c:in']);
        equal(graph.attacks.length, 3);
    });

    it('should give the graph of any stage', function () {
        const { send } = host();
        send({ type: 'solve', id: 4, theory: TWEETY, query: 'arg2p::solve', flags: FLAGS });
        const { solution } = send<'solution'>({ type: 'next', id: 4 });
        ok(solution.branch > 0);
        const graph = send<'graph'>({ type: 'graphAt', id: 4, branch: solution.branch, reqId: 9 });
        equal(graph.reqId, 9);
        deepEqual(graph.graph, solution.graph);
        deepEqual(send<'graph'>({ type: 'graphAt', id: 4, branch: 0, reqId: 10 }).graph.arguments, []);
    });

    it('should pass on what the solver prints', function () {
        const { posted, send } = host();
        send({ type: 'solve', id: 5, theory: '', query: 'write(hello)', flags: FLAGS });
        send({ type: 'next', id: 5 });
        ok(posted.some(it => it.type === 'output' && it.id === 5 && it.text.includes('hello')));
    });

    it('should tell where a text does not parse, and a query that is no longer running', function () {
        const { send } = host();
        const failed = send<'failed'>({ type: 'solve', id: 6, theory: 'a :=> b', query: 'arg2p::solve', flags: FLAGS });
        equal(failed.type, 'failed');
        equal(failed.error.syntax?.where, 'theory');
        equal(send({ type: 'next', id: 6 }).type, 'failed');
        equal(send<'failed'>({ type: 'solve', id: 7, theory: '', query: ' ', flags: FLAGS }).error.name, 'Query error');
    });

    it('should check texts', function () {
        equal(check(TWEETY, 'theory'), null);
        equal(check('flies(X)', 'query'), null);
        equal(check('', 'query'), null);
        const problem = check('a :=> b.\nc :=> d(.', 'theory')!;
        equal(problem.line, 2);
        ok(problem.column >= 1);
        ok(check('flies(', 'query') !== null);
    });
});

describe('Settings in effect', function () {
    it('should give the flags of the profile, unless the flags or the theory state others', function () {
        const aspic = settings(TWEETY, DEFAULT_FLAGS);
        deepEqual(aspic.graphBuildMode, ['aspicPlus']);
        deepEqual([...aspic.graphExtension].sort(), ['rebutRestriction', 'standardPref']);
        deepEqual(aspic.autoTransposition, ['true']);
        deepEqual(aspic.argumentLabellingMode, ['grounded']);

        const abstract = settings('graphBuildMode(abstract).\na -> b.', DEFAULT_FLAGS);
        deepEqual(abstract.graphBuildMode, ['abstract']);
        deepEqual(abstract.graphExtension, []);
        // the flags stated for the solver before the profile, which presets what they do not state
        deepEqual(abstract.queryMode, ['true']);
        deepEqual(settings('graphBuildMode(abstract).\na -> b.', '').queryMode, []);
        deepEqual(abstract.graphBuilder, ['abstractGraph']);

        deepEqual(settings(TWEETY, `${DEFAULT_FLAGS}autoTransposition(off).\n`).autoTransposition, []);
        deepEqual(settings('argumentLabellingMode(preferred).', DEFAULT_FLAGS).argumentLabellingMode, ['preferred']);
        deepEqual(settings(TWEETY, 'graphBuildMode(aba).\n').graphBuildMode, ['aba']);
    });

    it('should read the flags alone when the theory does not parse, and none when they do not', function () {
        deepEqual(settings('a :=> b', 'graphBuildMode(aba).\n').graphBuildMode, ['aba']);
        deepEqual(settings(TWEETY, 'graphBuildMode(aba'), {});
    });

    it('should know the profiles and the values of the flags of the API', function () {
        const values = (it: { values(): { value: string }[] }) => it.values().map(value => value.value).sort();
        const options = (name: string) => SCHEMA.find(it => it.name === name)!.options!.map(it => it.value).sort();
        deepEqual(PROFILES.map(it => it.name).sort(), values(api.Profile));
        deepEqual(options('argumentLabellingMode'), values(api.Semantics));
        deepEqual(options('statementLabellingMode'), values(api.StatementLabelling));
        deepEqual(options('graphExtension'), values(api.GraphExtension));
        deepEqual(options('answerMode'), values(api.AnswerMode));
        deepEqual(options('orderingPrinciple'), values(api.OrderingPrinciple));
        deepEqual(options('orderingComparator'), values(api.OrderingComparator));
        deepEqual(options('graphLanguage'), values(api.GraphLanguage));
        deepEqual(options('graphBuilder'), values(api.GraphBuilder));
        // the parts of the conflict model, and builtin, which stands for all of them: every value of the API is one of
        // them, and the parts are all of them once the API has them (arg2p after 0.22.0, which has builtin and
        // declared alone)
        const conflicts = SCHEMA.find(it => it.name === 'conflictModel')!;
        const known = [...options('conflictModel'), ...Object.keys(conflicts.aliases ?? {})].sort();
        ok(values(api.ConflictModel).every(it => known.includes(it)), values(api.ConflictModel).join());
        if (values(api.ConflictModel).includes('negation')) deepEqual(known, values(api.ConflictModel));
    });
});

describe('Engine of the browser', function () {
    it('should run queries through its channel', async function () {
        const run = await engine().solve({ theory: TWEETY, query: 'solve(In, Out, Und).', flags: FLAGS });
        equal(run.query, 'solve(In, Out, Und)');
        ok(run.hasNext());
        const first = await run.next();
        equal(first.result, 'yes');
        deepEqual(first.bindings.map(it => it.name), ['In', 'Out', 'Und']);
        const stage = await run.graphAt(first.branch);
        deepEqual(stage, first.graph);
    });

    it('should reject what does not parse, with where it is', async function () {
        const error = await engine()
            .solve({ theory: 'a :=> b', query: 'arg2p::solve', flags: FLAGS })
            .then(
                () => null,
                (it: unknown) => it as { name: string; syntax: { where: string } | null },
            );
        equal(error?.name, 'Theory error');
        equal(error?.syntax?.where, 'theory');
    });

    it('should stop what it is waited for, and end the queries of the worker it ends', async function () {
        const channels: Channel[] = [];
        const solving = engine(channels);
        const run = await solving.solve({ theory: TWEETY, query: 'solve', flags: FLAGS });
        const pending = run.next();
        solving.stop();
        equal((await failure(pending))?.stopped, true);
        equal(channels.length, 2);
        // its query went with the worker: no Next on it
        equal(run.hasNext(), false);
    });

    it('should end its worker when disposed, failing what was waited for, and start a fresh one when asked again', async function () {
        const channels: Channel[] = [];
        const solving = engine(channels);
        const run = await solving.solve({ theory: TWEETY, query: 'solve', flags: FLAGS });
        const pending = run.next();
        solving.dispose();
        ok((await failure(pending)) !== null);
        equal(run.hasNext(), false);
        equal(channels.length, 1, 'no fresh worker until asked');
        await solving.check('a.', 'theory');
        equal(channels.length, 2);
    });

    it('should ask the checks and the settings again of the worker it starts as it stops, failing the query alone', async function () {
        const channels: Channel[] = [];
        const solving = engine(channels);
        const run = await solving.solve({ theory: TWEETY, query: 'solve', flags: FLAGS });
        const pending = failure(run.next());
        const checked = solving.check('a :=> b', 'theory');
        const told = solving.settings('graphBuildMode(abstract).', DEFAULT_FLAGS);
        solving.stop();
        equal((await pending)?.stopped, true);
        equal((await checked)?.line, 1);
        deepEqual((await told).graphBuildMode, ['abstract']);
        equal(channels.length, 2);
    });

    it('should leave the worker be when it is stopped with checks alone waited for', async function () {
        const channels: Channel[] = [];
        const solving = engine(channels);
        const checked = solving.check(TWEETY, 'theory');
        solving.stop();
        equal(await checked, null);
        equal(channels.length, 1);
    });

    it('should read no stage of a query stopped, or replaced', async function () {
        const solving = engine();
        const run = await solving.solve({ theory: TWEETY, query: 'arg2p::solve', flags: FLAGS });
        const first = await run.next();
        deepEqual(await run.graphAt(first.branch), first.graph);
        // replaced while its stage is read
        const stage = failure(run.graphAt(0));
        const other = await solving.solve({ theory: TWEETY, query: 'arg2p::solve', flags: FLAGS });
        equal((await stage)?.name, 'Error');
        // stopped
        const pending = failure(other.next());
        solving.stop();
        equal((await pending)?.stopped, true);
        equal((await failure(other.graphAt(0)))?.name, 'Error');
    });

    it('should leave the worker be when it is stopped with nothing to stop', async function () {
        const channels: Channel[] = [];
        const solving = engine(channels);
        const run = await solving.solve({ theory: TWEETY, query: 'solve', flags: FLAGS });
        solving.stop();
        equal(channels.length, 1);
        ok(run.hasNext());
        equal((await run.next()).result, 'yes');
    });

    it('should give every solution asked for, even asked for at once', async function () {
        const run = await engine().solve({ theory: 'r1 :=> a.\nr2 :=> -a.\n', query: 'solve(In, Out, Und)', flags: FLAGS });
        const [first, second] = await Promise.all([run.next(), run.next()]);
        equal(first.result, 'yes');
        equal(second.result, 'yes');
        ok(JSON.stringify(first.bindings) !== JSON.stringify(second.bindings));
    });

    it('should pass on the output of the latest query alone, to where it was asked', async function () {
        const solving = engine();
        const [one, two]: string[][] = [[], []];
        const first = await solving.solve({ theory: '', query: 'write(one)', flags: FLAGS }, text => one.push(text));
        await first.next();
        const second = await solving.solve({ theory: '', query: 'write(two)', flags: FLAGS }, text => two.push(text));
        await second.next();
        deepEqual(one, ['one']);
        deepEqual(two, ['two']);
        // the query replaced has no more solutions
        equal(first.hasNext(), false);
    });

    it('should start a fresh worker after an error of its own, failing what was waited for', async function () {
        const channels: Channel[] = [];
        const solving = engine(channels);
        const pending = solving.solve({ theory: TWEETY, query: 'solve', flags: FLAGS });
        channels[0].onerror!({ message: 'out of memory', preventDefault: () => {} });
        equal((await failure(pending))?.name, 'Solver error');
        equal(channels.length, 2);
        const run = await solving.solve({ theory: TWEETY, query: 'solve', flags: FLAGS });
        equal((await run.next()).result, 'yes');
    });

    it('should give the settings in effect', async function () {
        deepEqual((await engine().settings('graphBuildMode(abstract).', DEFAULT_FLAGS)).graphBuildMode, ['abstract']);
    });

    it('should check texts', async function () {
        equal(await engine().check(TWEETY, 'theory'), null);
        equal((await engine().check('a :=> b', 'theory'))?.line, 1);
    });
});
