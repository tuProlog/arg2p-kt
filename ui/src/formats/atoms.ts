// The names of Prolog texts: atoms written as they are (a lowercase letter, then letters, digits or underscores), or
// quoted otherwise. The names of arguments, of flags and their values are atoms.

/** An atom written as it is, as the source of a regular expression */
export const ATOM = '[a-z][a-zA-Z0-9_]*';

const WHOLE_ATOM = new RegExp(`^${ATOM}$`);

/** Whether [text] is an atom written as it is */
export function isAtom(text: string): boolean {
    return WHOLE_ATOM.test(text);
}

/** [value] as an atom: as it is, or quoted */
export function quote(value: string): string {
    return isAtom(value) ? value : `'${value.replace(/'/g, "''")}'`;
}

/** The text of [value], an atom quoted or not */
export function unquote(value: string): string {
    const quoted = /^'(.*)'$/.exec(value);
    return quoted ? quoted[1].replace(/''/g, "'") : value;
}
