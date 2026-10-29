import { connect, type VsCode } from './federation.js';

// The federation view, in a webview of VS Code (see federation.ts), in the colours of its theme

declare function acquireVsCodeApi(): VsCode;

const style = document.createElement('style');
style.textContent = `
    body {
        padding: 4px 12px 12px;
        color: var(--vscode-foreground);
        font-family: var(--vscode-font-family);
        font-size: var(--vscode-font-size);
    }
    h3 {
        margin: 16px 0 6px;
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        color: var(--vscode-descriptionForeground);
    }
    ul {
        list-style: none;
        margin: 0;
        padding: 0;
    }
    li {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        justify-content: space-between;
        gap: 6px;
        padding: 4px 0;
    }
    .label {
        font-weight: 600;
        overflow-wrap: anywhere;
    }
    .muted {
        color: var(--vscode-descriptionForeground);
    }
    .error {
        width: 100%;
        margin: 2px 0;
        color: var(--vscode-errorForeground);
    }
    input:not([type='checkbox']) {
        box-sizing: border-box;
        padding: 3px 6px;
        color: var(--vscode-input-foreground);
        background: var(--vscode-input-background);
        border: 1px solid var(--vscode-input-border, transparent);
        font: inherit;
    }
    input:focus {
        outline: 1px solid var(--vscode-focusBorder);
    }
    .name {
        width: 100%;
    }
    .pin {
        width: 5em;
        letter-spacing: 0.2em;
    }
    .code {
        display: flex;
        align-items: center;
        gap: 12px;
        margin: 8px 0 4px;
    }
    .code span {
        font-family: var(--vscode-editor-font-family);
        font-size: 2em;
        letter-spacing: 0.25em;
    }
    button {
        padding: 3px 10px;
        color: var(--vscode-button-foreground);
        background: var(--vscode-button-background);
        border: none;
        cursor: pointer;
        font: inherit;
    }
    button:hover {
        background: var(--vscode-button-hoverBackground);
    }
    button.secondary {
        color: var(--vscode-button-secondaryForeground);
        background: var(--vscode-button-secondaryBackground);
    }
    .toggle {
        display: flex;
        gap: 6px;
        align-items: center;
        margin-top: 8px;
    }
    .qr svg {
        display: block;
        width: 180px;
        max-width: 100%;
        height: auto;
    }
    .url code {
        overflow-wrap: anywhere;
    }
`;
document.head.appendChild(style);

connect(document.getElementById('federation')!, acquireVsCodeApi(), window);
