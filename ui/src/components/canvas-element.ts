import { LitElement, css, html } from 'lit';
import type { Canvas } from '../graph/canvas.js';

/**
 * A component that draws on a canvas (the graph, the drawing of a framework): a bar over the area it draws in (an
 * element .area, holding the element .canvas), fitted to its room, drawn again in the colours of the page, and saved
 * as a picture, which there is none of while nothing is drawn.
 */
export abstract class CanvasElement extends LitElement {
    /** The name of the picture saved, with no extension */
    abstract fileName: string;

    protected abstract readonly canvas: Canvas;

    /** Fits the drawing to the element (after it was hidden, and so sized to nothing) */
    fit(): void {
        this.canvas.fit();
    }

    /** The drawing as a PNG picture, as a data URL: the whole of it, on the background of the element */
    png(): string {
        return this.canvas.png();
    }

    /** Draws again, in the colours the page sets now (after a change of theme) */
    restyle(): void {
        this.canvas.restyle();
    }

    // Whether nothing is drawn: no picture to save
    protected abstract get blank(): boolean;

    // The button that saves the picture of [what] (the graph, the framework)
    protected renderPng(what: string) {
        return html`
            <button type="button" class="png" title="Download the ${what} as a picture" ?disabled=${this.blank} @click=${() => this.canvas.export(this.fileName)}>
                <svg viewBox="0 0 24 24" width="14" height="14" aria-hidden="true"><path d="M12 3v12m0 0l-5-5m5 5l5-5M4 19h16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" /></svg>
                PNG
            </button>
        `;
    }
}

/** The look of a component that draws: its bar, and its area filled by the canvas */
export const canvasStyles = css`
    :host {
        display: flex;
        flex-direction: column;
        min-height: 200px;
        background: var(--_surface);
        font-size: 13px;
    }
    .bar {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        padding: 6px 10px;
        border-bottom: 1px solid var(--_border);
    }
    .area {
        flex: 1;
        min-height: 0;
        position: relative;
        overflow: hidden;
    }
    .canvas {
        position: absolute;
        inset: 0;
    }
`;
