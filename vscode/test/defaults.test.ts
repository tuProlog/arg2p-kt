import { DEFAULT_FLAGS } from '@tuprolog/arg2p-ui/data';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { assert, describe, it } from 'vitest';
import manifest from '../package.json' with { type: 'json' };
import { FLAGS, JAVA, STACK } from '../src/defaults.js';

const { equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

// The version [key] of the catalog of the build of arg2p
function catalog(key: string): string | undefined {
    const text = readFileSync(join(__dirname, '..', '..', 'gradle', 'libs.versions.toml'), 'utf8');
    return new RegExp(`^${key} = "([^"]+)"$`, 'm').exec(text)?.[1];
}

// The defaults of the extension, as the code reads them, and what states them elsewhere: the same
describe('The defaults of the extension', function () {
    it('should solve with the flags of the solver page', function () {
        equal(FLAGS, DEFAULT_FLAGS);
    });

    it('should run the language server with a stack of its own', function () {
        ok(/^-Xss\d+[kmg]$/i.test(STACK), STACK);
    });

    it('should ask for the Java arg2p targets, as its settings and README say', function () {
        equal(catalog('jvm'), String(JAVA));
        ok(manifest.contributes.configuration.properties['arg2p.java.path'].markdownDescription.includes(`(${JAVA} or later)`));
        ok(readFileSync(join(__dirname, '..', 'README.md'), 'utf8').includes(`(${JAVA} or later)`));
    });

    it('should build with the components of the solver page the language server loads', function () {
        equal(manifest.devDependencies['@tuprolog/arg2p-ui'], catalog('arg2pUi'));
    });
});
