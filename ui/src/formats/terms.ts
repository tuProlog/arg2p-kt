// Terms of Prolog as solutions write them.

/** The top level elements of a Prolog list term, or null for any other term (a list with a tail, `[a | T]`, too) */
export function listItems(term: string): string[] | null {
    const text = term.trim();
    if (!text.startsWith('[') || !text.endsWith(']')) return null;
    const body = text.slice(1, -1).trim();
    if (body === '') return [];
    const items: string[] = [];
    let depth = 0;
    let quoted = false;
    let current = '';
    for (const char of body) {
        if (char === "'") quoted = !quoted;
        if (!quoted && '([{'.includes(char)) depth++;
        if (!quoted && ')]}'.includes(char)) depth--;
        if (!quoted && depth === 0 && char === '|') return null;
        if (!quoted && depth === 0 && char === ',') {
            items.push(current.trim());
            current = '';
        } else {
            current += char;
        }
    }
    items.push(current.trim());
    return items;
}
