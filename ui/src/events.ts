// How the components, and what they draw on, tell the page what happens: events that cross the components they are in.

/** Tells the page [type], with [detail]: the event crosses the components it is in. Whether no one cancelled it (when [cancelable]) */
export function emit(element: HTMLElement, type: string, detail: unknown, cancelable = false): boolean {
    return element.dispatchEvent(new CustomEvent(type, { detail, bubbles: true, composed: true, cancelable }));
}
