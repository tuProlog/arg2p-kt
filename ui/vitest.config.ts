import { defineConfig } from 'vitest/config';

// The engine of the browser runs in Node.js as it is; the tests of the components ask for a simulated DOM themselves
// (// @vitest-environment happy-dom).
export default defineConfig({
    test: {
        include: ['test/**/*.test.ts'],
        environment: 'node',
        testTimeout: 60_000,
    },
});
