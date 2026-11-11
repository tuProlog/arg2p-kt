import { defineConfig } from 'vite';

// The solver in one file, its dependencies within (CodeMirror, Lit, Cytoscape): for the pages that load it from a CDN
// as it is, where each package is built on its own (jsDelivr) and the editors would get copies of CodeMirror they
// refuse. It is the solver tsc built: its components register themselves as their imports run. It has no engine (the
// page gives its own), and so nothing of @tuprolog/arg2p.
//
// Minified whole: a library of ES modules is minified without its whitespace, and keeps the annotations a bundler
// reads, which a file loaded as it is does not need; the notices of the licences are kept.
export default defineConfig({
    build: {
        lib: {
            entry: 'dist/components/arg2p-solver.js',
            formats: ['es'],
            fileName: () => 'solver.js',
        },
        outDir: 'dist/bundle',
        emptyOutDir: true,
        rolldownOptions: { output: { minify: true, comments: { legal: true, annotation: false } } },
    },
});
