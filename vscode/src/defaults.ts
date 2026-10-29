import manifest from '../package.json' with { type: 'json' };

// The defaults of the extension, in one place: those of its settings as package.json declares them, and the others. The
// extension and the preview of the solver (test/preview) read them here.

const settings = manifest.contributes.configuration.properties;

/** The flags a theory is first solved with: the default of the setting arg2p.flags */
export const FLAGS: string = settings['arg2p.flags'].default;

/** The stack of the JVM running the language server, which resolution needs deep: that of the default of arg2p.java.options */
export const STACK: string = settings['arg2p.java.options'].default.find(it => it.startsWith('-Xss'))!;

/** The query a theory is first solved with */
export const QUERY = 'arg2p::solve(In, Out, Und)';

/** The oldest Java the language server runs on: the JVM arg2p targets (jvm in gradle/libs.versions.toml) */
export const JAVA = 21;
