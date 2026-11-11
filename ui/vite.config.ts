import { defineConfig } from 'vite';

// The page the components are tried on while they are written (npm run dev)
export default defineConfig({
    root: 'demo',
    worker: { format: 'es' },
});
