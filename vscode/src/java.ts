import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { STACK } from './defaults.js';

// The java running the language server: the one the settings name, or the runtime the extension bundles (a package
// for a platform has one), or the one of JAVA_HOME, or java on the path

/** The java of [setting], or else of [extension]/runtime, or else of [javaHome], or else `java` */
export function javaOf(setting: string, extension: string, javaHome: string | undefined, platform: NodeJS.Platform = process.platform): string {
    if (setting.trim() !== '') return setting.trim();
    const executable = platform === 'win32' ? 'java.exe' : 'java';
    const bundled = join(extension, 'runtime', 'bin', executable);
    if (existsSync(bundled)) return bundled;
    if (javaHome !== undefined && javaHome !== '') {
        const home = join(javaHome, 'bin', executable);
        if (existsSync(home)) return home;
    }
    return 'java';
}

/** The arguments of java running the server [jar]: the [options] of the settings, and the stack resolution needs */
export function argumentsOf(options: readonly string[], jar: string): string[] {
    const stack = options.some(it => it.startsWith('-Xss')) ? [] : [STACK];
    return [...stack, ...options, '-jar', jar];
}
