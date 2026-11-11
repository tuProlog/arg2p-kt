import { HighlightStyle, StreamLanguage, syntaxHighlighting, type StreamParser } from '@codemirror/language';
import type { Extension } from '@codemirror/state';
import { tags } from '@lezer/highlight';

// The language of arg2p theories, queries and flags for the editor: Prolog, with the arrows of its rules and of
// abstract frameworks (`=>`, `->`, `:=>`, `<->`, ...) set apart.

interface State {
    comment: boolean;
}

const parser: StreamParser<State> = {
    name: 'arg2p',
    startState: () => ({ comment: false }),
    token(stream, state) {
        if (state.comment) {
            if (stream.skipTo('*/')) {
                stream.pos += 2;
                state.comment = false;
            } else {
                stream.skipToEnd();
            }
            return 'comment';
        }
        if (stream.eatSpace()) return null;
        if (stream.match('/*')) {
            state.comment = true;
            return 'comment';
        }
        if (stream.match(/^%.*/)) return 'comment';
        if (stream.match(/^[a-z][a-zA-Z0-9_]*(?=\()/)) return 'functor';
        if (stream.match(/^[a-z][a-zA-Z0-9_]*/)) return 'atom';
        if (stream.match(/^[A-Z_][a-zA-Z0-9_]*/)) return 'variable';
        if (stream.match(/^"([^"\\]|\\.)*"?/)) return 'string';
        if (stream.match(/^'([^'\\]|\\.|'')*'?/)) return 'atom';
        if (stream.match(/^0'(\\.|.)/)) return 'number';
        if (stream.match(/^\d+(\.\d+)?([eE][-+]?\d+)?/)) return 'number';
        if (stream.match(/^(:=>|:->|<->|=>|->|:-|\?-)(?![=><!~?:&|+\-*/^\\@#$.])/)) return 'arrow';
        if (stream.match(/^\.(?=\s|%|$)/)) return 'end';
        if (stream.match(/^[=><!~?:&|+\-*/^\\@#$.]+/)) return 'operator';
        if (stream.match(/^[(),;[\]{}|]/)) return 'punctuation';
        stream.next();
        return null;
    },
    languageData: { commentTokens: { line: '%', block: { open: '/*', close: '*/' } } },
    tokenTable: {
        comment: tags.comment,
        functor: tags.function(tags.name),
        atom: tags.atom,
        variable: tags.variableName,
        string: tags.string,
        number: tags.number,
        arrow: tags.keyword,
        end: tags.separator,
        operator: tags.operator,
        punctuation: tags.punctuation,
    },
};

export const arg2pLanguage = StreamLanguage.define(parser);

// The colours of the tokens, custom properties the page may set (see theme.ts)
const style = HighlightStyle.define([
    { tag: tags.comment, color: 'var(--_comment)', fontStyle: 'italic' },
    { tag: [tags.atom, tags.number, tags.string], color: 'var(--_atom)' },
    { tag: tags.function(tags.name), color: 'var(--_functor)' },
    { tag: tags.variableName, color: 'var(--_variable)' },
    { tag: tags.keyword, color: 'var(--_accent)', fontWeight: 'bold' },
    { tag: [tags.operator, tags.separator, tags.punctuation], color: 'var(--_muted)' },
]);

/** The language and its colours */
export function arg2p(): Extension {
    return [arg2pLanguage, syntaxHighlighting(style)];
}
