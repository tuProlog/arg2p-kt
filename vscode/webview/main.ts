import '@tuprolog/arg2p-ui/solver';
import { connect, type VsCode } from './solver.js';

// The solver of a theory, in the webview of VS Code: <arg2p-solver> of @tuprolog/arg2p-ui, the whole height of the
// webview, in the colours of the theme of VS Code, bound to the extension (see solver.ts).

declare function acquireVsCodeApi(): VsCode;

const solver = document.querySelector('arg2p-solver')!;
connect(solver, acquireVsCodeApi(), window);

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
