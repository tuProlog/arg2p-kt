import cytoscape, { type Core, type EventObject, type StylesheetJson } from 'cytoscape';
import type { LitElement, ReactiveController } from 'lit';
import { emit } from '../events.js';
import { cssColor, labelColor } from '../theme.js';
import { MUTUAL_STYLE, markMutual, placeByName, placesAll, positionsOf, type Positions } from './positions.js';

// What the components draw on: a Cytoscape instance in their container (an element .canvas, in an element .area with
// the popup .popup), made and ended with the component. Where nothing can be drawn (no canvas, as in a simulated DOM),
// it is headless: it keeps the elements, their positions and their events alike, and draws nothing.

// The font the nodes are written in, and how their size follows from what they say
const NODE_FONT = { family: 'arial, sans-serif', size: 14 };
const LINE_HEIGHT = 20;
const PADDING_X = 24;
const PADDING_Y = 14;
const CHARACTER_WIDTH = 8;

let measure: CanvasRenderingContext2D | null | undefined;

function canvas(): CanvasRenderingContext2D | null {
    if (measure === undefined) {
        try {
            measure = document.createElement('canvas').getContext('2d');
        } catch {
            measure = null;
        }
        if (measure) measure.font = `${NODE_FONT.size}px ${NODE_FONT.family}`;
    }
    return measure;
}

// Nodes and edges take their sizes, what they say and their colours from their data: the colours from their tone
const STYLE: StylesheetJson = [
    {
        selector: 'node',
        style: {
            shape: 'ellipse',
            'background-color': 'data(color)',
            label: 'data(text)',
            // the labels are written on their colours alike in every theme
            color: '#ffffff',
            'font-family': NODE_FONT.family,
            'font-size': NODE_FONT.size,
            'text-valign': 'center',
            'text-halign': 'center',
            'text-wrap': 'wrap',
            width: 'data(width)',
            height: 'data(height)',
        },
    },
    { selector: 'node:selected', style: { 'border-width': 3, 'border-color': 'data(outline)' } },
    // the arguments a page links to what it picked (see the property linked of the components)
    { selector: 'node.linked', style: { 'border-width': 4, 'border-style': 'double', 'border-color': 'data(outline)', 'overlay-opacity': 0.12, 'overlay-color': 'data(outline)', 'overlay-padding': 6 } },
    {
        selector: 'edge',
        style: {
            width: 2,
            'line-color': 'data(color)',
            'target-arrow-color': 'data(color)',
            'target-arrow-shape': 'triangle',
            'curve-style': 'bezier',
        },
    },
];

/** A Cytoscape instance drawing in [container], or a headless one where nothing can be drawn */
function drawing(container: HTMLElement, style: StylesheetJson): Core {
    const headless = canvas() === null;
    return cytoscape({
        ...(headless ? { headless: true, styleEnabled: true } : { container }),
        style,
        minZoom: 0.1,
        maxZoom: 4,
    });
}

// A small drawing fitted to its area is not blown up past this zoom
const FIT_ZOOM = 1.25;

/** [cy] fitted to its area, no closer than FIT_ZOOM */
function fit(cy: Core): void {
    cy.fit(undefined, 30);
    if (cy.zoom() > FIT_ZOOM) {
        cy.zoom(FIT_ZOOM);
        cy.center();
    }
}

/** The size of an ellipse that holds [label] (lines split at \n) */
export function nodeSize(label: string): { width: number; height: number } {
    const lines = label.split('\n');
    const context = canvas();
    const text = Math.max(...lines.map(it => context?.measureText(it).width ?? it.length * CHARACTER_WIDTH));
    const height = lines.length * LINE_HEIGHT + PADDING_Y * 2;
    return { width: Math.max(height, text + PADDING_X * 2), height };
}

// What a pointer does on the drawing: cytoscape reads where the drawing is on the screen as it handles it
const POINTER_EVENTS = ['mousedown', 'mousemove', 'touchstart', 'wheel'] as const;

/**
 * Keeps [cy] the size of [container] (a page may hide it, or lay it out again), fitting the drawing once it is shown
 * after being hidden, and where it is on the screen (a page scrolled moves it: cytoscape keeps where it was, and a tap
 * would miss what it is on). Gives what stops it.
 */
function followSize(cy: Core, container: HTMLElement): () => void {
    if (typeof ResizeObserver === 'undefined' || cy.container() === null) return () => {};
    let hidden = container.clientWidth === 0 || container.clientHeight === 0;
    const observer = new ResizeObserver(() => {
        const nowHidden = container.clientWidth === 0 || container.clientHeight === 0;
        cy.resize();
        if (hidden && !nowHidden && cy.elements().nonempty()) fit(cy);
        hidden = nowHidden;
    });
    observer.observe(container);
    // before cytoscape handles the event: listened to while it goes down to the drawing, above its own listeners
    const above = container.parentElement ?? container;
    const renderer = (cy as unknown as { renderer(): { invalidateContainerClientCoordsCache?: () => void } }).renderer();
    const moved = () => (renderer.invalidateContainerClientCoordsCache === undefined ? cy.resize() : renderer.invalidateContainerClientCoordsCache());
    POINTER_EVENTS.forEach(it => above.addEventListener(it, moved, { capture: true, passive: true }));
    return () => {
        observer.disconnect();
        POINTER_EVENTS.forEach(it => above.removeEventListener(it, moved, { capture: true }));
    };
}

/**
 * A node or an edge to draw. Its data has its id and its tone, the label it is coloured by (in, out, und, na) or
 * support; a node has its name too (by which it is placed, see Positions), what it says (text), and its size.
 */
export interface Drawn {
    readonly group: 'nodes' | 'edges';
    readonly data: { readonly id: string; readonly tone: string; readonly [key: string]: unknown };
    readonly classes: string;
}

export interface CanvasOptions {
    /** The style of the component, over that of every drawing */
    readonly style: StylesheetJson;
    /** Draws everything again, on a drawing made anew (the component was put back in a page) */
    readonly redraw: () => void;
    /** Listens to the drawing for what the component does with it */
    readonly setup?: (cy: Core) => void;
    /** Whether a tap pins the popup of the argument it is on (or closes it, off the arguments); always, by default */
    readonly pins?: () => boolean;
    /** Whether the arguments stay where they are, not dragged; never, by default */
    readonly locked?: () => boolean;
}

type Host = LitElement;

const drawings = new WeakMap<Element, Canvas>();

/** The drawing of [element], a component that draws: for tests, it is no part of the API */
export function drawingOf(element: Element): Core | null {
    return drawings.get(element)?.cy ?? null;
}

/**
 * The drawing of a component, from its first update to its removal from the page (made anew if it is put back): the
 * elements drawn kept in place as they change, coloured by the tokens of the theme, and saved as a picture. Hovering
 * an argument shows its popup, tapping it keeps it open. Drawn without a layout of their own, the arguments are placed
 * by name where the page has them, and where they are is told (arg2p-positions) as they are laid out or dragged.
 */
export class Canvas implements ReactiveController {
    private core: Core | null = null;
    private unfollow = () => {};
    // the argument the popup is about, and the one a tap keeps it open on
    private shownId: string | null = null;
    private pinned: string | null = null;
    // whether the arguments are placed by name, and where they are, as last told or given
    private shared = false;
    private places: Positions | null = null;

    constructor(
        private readonly host: Host,
        private readonly options: CanvasOptions,
    ) {
        host.addController(this);
        drawings.set(host, this);
    }

    /** The drawing, once the component is first drawn and while it is in a page */
    get cy(): Core | null {
        return this.core;
    }

    /** The argument (its id) the popup is about */
    get shown(): string | null {
        return this.shownId;
    }

    hostConnected(): void {
        // put back in a page after being taken out of it
        if (this.host.hasUpdated && this.core === null) {
            this.create();
            this.options.redraw();
        }
    }

    hostUpdated(): void {
        // updated out of a page: drawn once put back in one (a drawing made now would never be ended)
        if (!this.host.isConnected) return;
        if (this.core === null) this.create();
        this.core!.autoungrabify(this.options.locked?.() ?? false);
        this.placePopup();
    }

    hostDisconnected(): void {
        this.unfollow();
        this.core?.destroy();
        this.core = null;
    }

    /**
     * Draws [elements]: those no longer there removed, the others changed in place (they keep their places), the new
     * ones added. [layout] places them all; without one, they are placed by name (see Positions). Fitted to the area
     * when drawn first.
     */
    draw(elements: readonly Drawn[], layout?: (cy: Core) => void): void {
        const cy = this.core;
        if (cy === null) return;
        const before = new Set(cy.nodes().map(it => it.id()));
        const palette = this.palette();
        const wanted = new Set(elements.map(it => it.data.id));
        cy.batch(() => {
            cy.elements().filter(it => !wanted.has(it.id())).remove();
            elements.forEach(({ group, data, classes }) => {
                const element = cy.getElementById(data.id);
                const coloured = { ...data, ...palette(data.tone) };
                if (element.nonempty()) element.data(coloured).classes(classes);
                else cy.add({ group, data: coloured, classes });
            });
        });
        markMutual(cy);
        this.shared = layout === undefined;
        if (layout !== undefined) layout(cy);
        else this.arrange(before);
        if (before.size === 0 && cy.nodes().nonempty()) fit(cy);
        // the popup of an argument no longer drawn is gone with it
        if (this.shownId !== null && cy.getElementById(this.shownId).empty()) this.shownId = this.pinned = null;
    }

    /** Gives the class [name] to the arguments whose statement [test] picks, and takes it from the others: nothing else changes */
    mark(name: string, test: (statement: string) => boolean): void {
        const cy = this.core;
        if (cy === null) return;
        cy.batch(() => cy.nodes().forEach(it => void it.toggleClass(name, test((it.data('statement') as string | undefined) ?? ''))));
    }

    /** Adds [element] at [position], ahead of drawing it (an argument drawn goes where it was asked for) */
    add(element: Drawn, position: { x: number; y: number }): void {
        this.core?.add({ group: element.group, data: { ...element.data, ...this.palette()(element.data.tone) }, classes: element.classes, position: { ...position } });
    }

    /** Places the arguments where the page has them (the other drawing of the framework moved them) */
    give(positions: Positions | null): void {
        this.places = positions;
        const cy = this.core;
        if (positions !== null && cy !== null && this.shared && cy.nodes().nonempty() && placeByName(cy, positions)) this.tell();
    }

    /** Colours the drawing again with the tokens of the theme (after the page changed theme) */
    restyle(): void {
        const cy = this.core;
        if (cy === null) return;
        const palette = this.palette();
        cy.batch(() => cy.elements().forEach(it => void it.data(palette(it.data('tone') as string))));
    }

    /** Fits the drawing to its area (after the area was hidden, and so sized to nothing) */
    fit(): void {
        if (this.core === null) return;
        this.core.resize();
        fit(this.core);
    }

    /** The whole drawing as a PNG picture, as a data URL, on the background of the component */
    png(): string {
        const cy = this.core;
        if (cy === null || cy.container() === null) return '';
        return cy.png({ full: true, bg: cssColor(this.host, 'surface'), scale: 2, output: 'base64uri' });
    }

    /** Saves the drawing as [name].png: told first (arg2p-export), the page cancelling it to save it another way */
    export(name: string): void {
        const fileName = `${name}.png`;
        const dataUrl = this.png();
        if (!emit(this.host, 'arg2p-export', { fileName, dataUrl }, true)) return;
        const link = document.createElement('a');
        link.href = dataUrl;
        link.download = fileName;
        link.click();
    }

    private create(): void {
        const container = this.host.renderRoot.querySelector<HTMLElement>('.canvas')!;
        const cy = drawing(container, [...STYLE, ...this.options.style, MUTUAL_STYLE]);
        // the page is told the argument hovered, and its statement, as it is when one is tapped (arg2p-argument-hover)
        cy.on('mouseover', 'node', (event: EventObject) => {
            this.show(event.target.id());
            emit(this.host, 'arg2p-argument-hover', { id: event.target.id() as string, statement: (event.target.data('statement') as string | undefined) ?? null });
        });
        cy.on('mouseout', 'node', () => {
            this.show(this.pinned);
            emit(this.host, 'arg2p-argument-hover', { id: null, statement: null });
        });
        // a tap is the only way to show the popup on touch screens; it picks the argument too (or none, off them): the
        // page is told which, and the statement of the theory it comes from (arg2p-argument-select)
        cy.on('tap', (event: EventObject) => {
            if (!(this.options.pins?.() ?? true)) return;
            const node = event.target !== cy && event.target.isNode() ? event.target : null;
            this.pinned = node === null ? null : (node.id() as string);
            this.show(this.pinned);
            emit(this.host, 'arg2p-argument-select', { id: this.pinned, statement: node === null ? null : ((node.data('statement') as string | undefined) ?? null) });
        });
        // the popup follows its node while the drawing is dragged or zoomed
        cy.on('viewport position', () => this.placePopup());
        cy.on('dragfree', 'node', () => {
            if (this.shared) this.tell();
        });
        cy.autoungrabify(this.options.locked?.() ?? false);
        this.options.setup?.(cy);
        this.core = cy;
        this.unfollow = followSize(cy, container);
    }

    // The arguments new to the drawing (not among [before]) placed where the places shared have them, or laid out
    // around the others: told when the places shared were not those of every argument
    private arrange(before: ReadonlySet<string>): void {
        const cy = this.core!;
        const kept = positionsOf(cy.nodes().filter(it => before.has(it.id())));
        const laid = placeByName(cy, { ...this.places, ...kept });
        if (laid || !placesAll(cy, this.places)) this.tell();
    }

    // Where the arguments are now, for the other drawing to place them alike
    private tell(): void {
        this.places = positionsOf(this.core!.nodes());
        emit(this.host, 'arg2p-positions', { positions: this.places });
    }

    private show(id: string | null): void {
        if (id === this.shownId) return;
        this.shownId = id;
        this.host.requestUpdate();
    }

    // The colours of the tones, as the component is styled now
    private palette(): (tone: string) => { color: string; outline: string } {
        const host = this.host;
        const colours: Record<string, string> = {
            in: labelColor(host, 'in'),
            out: labelColor(host, 'out'),
            und: labelColor(host, 'und'),
            na: labelColor(host, 'na'),
            support: cssColor(host, 'support'),
        };
        const outline = cssColor(host, 'accent');
        return tone => ({ color: colours[tone] ?? colours.na, outline });
    }

    // The popup next to its node, flipped to the other side or held in when it would leave the area
    private placePopup(): void {
        const popup = this.host.renderRoot.querySelector<HTMLElement>('.popup');
        const area = this.host.renderRoot.querySelector<HTMLElement>('.area');
        const node = this.shownId === null ? null : this.core?.getElementById(this.shownId);
        if (popup === null || area === null || node == null || node.empty()) return;
        const point = node.renderedPosition();
        const margin = 8;
        const gap = node.renderedWidth() / 2 + 8;
        const width = popup.offsetWidth;
        const height = popup.offsetHeight;
        let left = point.x + gap;
        if (left + width > area.clientWidth - margin) left = point.x - gap - width;
        left = Math.max(margin, Math.min(left, area.clientWidth - width - margin));
        const top = Math.max(margin, Math.min(point.y - height / 2, area.clientHeight - height - margin));
        popup.style.left = `${left}px`;
        popup.style.top = `${top}px`;
    }
}
