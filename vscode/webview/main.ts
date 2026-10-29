import '@tuprolog/arg2p-ui/solver';
import { connect, type VsCode } from './solver.js';

// The solver of a theory, in the webview of VS Code: <arg2p-solver> of @tuprolog/arg2p-ui, the whole height of the
// webview, in the colours of the theme of VS Code, bound to the extension (see solver.ts), with the switch Parallel
// among its actions.

declare function acquireVsCodeApi(): VsCode;

const solver = document.querySelector('arg2p-solver')!;
const parallel = document.createElement('label');
parallel.slot = 'actions';
parallel.className = 'parallel';
parallel.title = 'Solve on the federation: the workers of this device, and of the devices linked to it (see the view Arg2P Federation)';
const toggle = document.createElement('input');
toggle.type = 'checkbox';
parallel.append(toggle, 'Parallel');
solver.append(parallel);
connect(solver, acquireVsCodeApi(), window, toggle);

// The colours of the components, those of the theme of VS Code
const theme = document.createElement('style');
theme.textContent = `
    :root {
        --arg2p-surface: var(--vscode-editor-background);
        --arg2p-surface-2: var(--vscode-sideBar-background, var(--vscode-editor-background));
        --arg2p-hover: var(--vscode-list-hoverBackground);
        --arg2p-border: var(--vscode-panel-border, var(--vscode-widget-border, #8884));
        --arg2p-text: var(--vscode-foreground);
        --arg2p-muted: var(--vscode-descriptionForeground);
        --arg2p-accent: var(--vscode-textLink-foreground);
        --arg2p-error: var(--vscode-errorForeground);
        --arg2p-font: var(--vscode-font-family);
        --arg2p-mono: var(--vscode-editor-font-family);
        --arg2p-atom: var(--vscode-debugTokenExpression-string, #ce9178);
        --arg2p-functor: var(--vscode-symbolIcon-functionForeground, #dcdcaa);
        --arg2p-comment: var(--vscode-editorLineNumber-foreground);
    }
    body {
        margin: 0;
        padding: 0;
        background: var(--vscode-editor-background);
    }
    .parallel {
        display: inline-flex;
        align-items: center;
        gap: 4px;
        margin: 0 8px;
        color: var(--vscode-descriptionForeground);
        cursor: pointer;
        user-select: none;
    }
    arg2p-solver {
        height: 100vh;
        padding: 8px 12px;
        font-size: var(--vscode-font-size, 13px);
    }
`;
document.head.appendChild(theme);

// Another theme, whose colours VS Code sets on the page as it marks the body with its kind: the graph and the drawing
// are drawn again in them
new MutationObserver(() => solver.restyle()).observe(document.body, { attributes: true, attributeFilter: ['class'] });
