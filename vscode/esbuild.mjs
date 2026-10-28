import * as esbuild from 'esbuild';

// The two bundles of the extension: the extension itself (Node.js, CommonJS, vscode left to VS Code) and the page of
// the solver in its webview (a browser, with the components of @tuprolog/arg2p-ui in it). Everything they use is in
// them: the package has no node_modules.

const production = process.argv.includes('--production');
const watch = process.argv.includes('--watch');

const common = { bundle: true, minify: production, sourcemap: !production, logLevel: 'info' };

const contexts = await Promise.all([
    esbuild.context({
        ...common,
        entryPoints: ['src/extension.ts'],
        outfile: 'dist/extension.js',
        platform: 'node',
        format: 'cjs',
        target: 'node20',
        external: ['vscode'],
    }),
    esbuild.context({
        ...common,
        entryPoints: ['webview/main.ts'],
        outfile: 'dist/webview.js',
        // one Lit, the page's, for the components too
        alias: { lit: './node_modules/lit' },
        platform: 'browser',
        format: 'esm',
        target: 'es2022',
    }),
]);

if (watch) {
    await Promise.all(contexts.map(it => it.watch()));
} else {
    await Promise.all(contexts.map(it => it.rebuild()));
    await Promise.all(contexts.map(it => it.dispose()));
}
