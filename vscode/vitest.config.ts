import { defineConfig } from 'vitest/config';

// The bridge and the lookup of java on their own; the language server, through JSON-RPC, once its jar is built
export default defineConfig({
    test: {
        include: ['test/**/*.test.ts'],
        environment: 'node',
        testTimeout: 120_000,
        hookTimeout: 120_000,
    },
});
