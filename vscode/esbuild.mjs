import * as esbuild from 'esbuild';

// The bundles of the extension: the extension itself (Node.js, CommonJS, vscode left to VS Code), the page of the
// solver in its webview (a browser, with the components of @tuprolog/arg2p-ui in it) and the page of the federation
// view. Everything they use is in them: the package has no node_modules.

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
        entryPoints: { webview: 'webview/main.ts', federation: 'webview/federationMain.ts' },
        outdir: 'dist',
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
