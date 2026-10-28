import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { assert, describe, it } from 'vitest';
import { argumentsOf, javaOf } from '../src/java.js';

const { deepEqual, equal } = assert;

// A directory with an executable named java in bin/ under [below]
function withJava(below: string, executable = 'java'): string {
    const root = mkdtempSync(join(tmpdir(), 'arg2p-java-'));
    mkdirSync(join(root, below, 'bin'), { recursive: true });
    writeFileSync(join(root, below, 'bin', executable), '');
    return root;
}

describe('Java', function () {
    it('should take the java of the settings first', function () {
        equal(javaOf(' /opt/jdk/bin/java ', withJava('runtime'), undefined, 'linux'), '/opt/jdk/bin/java');
    });

    it('should take the runtime the extension bundles, then JAVA_HOME, then java on the path', function () {
        const extension = withJava('runtime');
        equal(javaOf('', extension, undefined, 'linux'), join(extension, 'runtime', 'bin', 'java'));
        const home = withJava('');
        equal(javaOf('', mkdtempSync(join(tmpdir(), 'arg2p-none-')), home, 'linux'), join(home, 'bin', 'java'));
        equal(javaOf('', mkdtempSync(join(tmpdir(), 'arg2p-none-')), undefined, 'linux'), 'java');
        const windows = withJava('runtime', 'java.exe');
        equal(javaOf('', windows, undefined, 'win32'), join(windows, 'runtime', 'bin', 'java.exe'));
    });

    it('should give the server a deep stack unless the settings set one', function () {
        deepEqual(argumentsOf(['-Xmx8g'], 'server.jar'), ['-Xss512m', '-Xmx8g', '-jar', 'server.jar']);
        deepEqual(argumentsOf(['-Xss1g'], 'server.jar'), ['-Xss1g', '-jar', 'server.jar']);
    });
});
