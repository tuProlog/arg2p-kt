import { renderSVG } from 'uqr';
import type { FederationState, FromFederation, ToFederation } from '../src/protocol.js';

// The page of the federation view: this device (its name, its code), the devices nearby to link to by their code, the
// devices linked with their workers, and the QR code of the page a phone joins from. What the user does is told to the
// extension (see federation.ts there), which tells the page the state of the federation as it changes.

/** The extension, as the page speaks to it */
export interface VsCode {
    postMessage(message: FromFederation): void;
}

/** What the page shows beside the state: the codes typed for the devices nearby, and why a request failed */
export interface Shown {
    readonly codes: ReadonlyMap<string, string>;
    readonly failure: { readonly site: string | null; readonly message: string } | null;
}

/** The view of [state] (null: the language server does not run), as HTML */
export function view(state: FederationState | null, shown: Shown): string {
    if (state === null) return '<p class="muted">The language server of Arg2P is not running.</p>';
    const failure = (site: string | null) =>
        shown.failure !== null && shown.failure.site === site ? `<p class="error" role="alert">${escape(shown.failure.message)}</p>` : '';
    const enabled = `<label class="toggle"><input type="checkbox" data-action="enable"${state.enabled ? ' checked' : ''}> Federate with the devices nearby</label>`;
    if (!state.enabled) {
        return `${enabled}<p class="muted">Parallel queries run on the ${plural(state.workers, 'worker')} of this device alone.</p>${failure(null)}`;
    }
    const device = `
        <section>
            <h3>This device</h3>
            <input class="name" data-action="rename" value="${escape(state.name)}" aria-label="Name of this device" title="The name the other devices see">
            <div class="code"><span title="The code the other devices type to link to this one">${escape(state.code ?? '')}</span>
                <button class="secondary" data-action="newCode" title="Refuse the code shown from now on (the devices linked stay)">New code</button></div>
            <p class="muted">${plural(state.workers, 'worker')}. Give the code to the device that links to this one.</p>
        </section>`;
    const nearby = state.nearby.length === 0
        ? '<p class="muted">None yet: they show here once they run Arg2P on the same network.</p>'
        : state.nearby
              .map(
                  site => `
            <li>
                <span class="label">${escape(site.name || site.id)}</span>
                <span class="link"><input class="pin" data-site="${escape(site.id)}" inputmode="numeric" maxlength="4" placeholder="Code" aria-label="Code of ${escape(site.name || site.id)}" value="${escape(shown.codes.get(site.id) ?? '')}">
                <button data-action="pair" data-site="${escape(site.id)}">Link</button></span>
                ${failure(site.id)}
            </li>`,
              )
              .join('');
    const linked = state.sites.length === 0
        ? '<p class="muted">None.</p>'
        : state.sites
              .map(site => {
                  const workers = site.workers === null || site.workers === undefined ? 'workers unknown' : plural(site.workers, 'worker');
                  const via = site.via ? `, through ${escape(nameOf(state, site.via))}` : '';
                  return `<li><span class="label">${escape(site.name ?? 'A browser')}</span><span class="muted">${workers}${via}</span></li>`;
              })
              .join('');
    const phone = state.joinUrl
        ? `<div class="qr">${renderSVG(state.joinUrl, { ecc: 'M', border: 2, whiteColor: '#ffffff', blackColor: '#000000' })}</div>
           <p class="url"><code>${escape(state.joinUrl)}</code></p>
           <p class="muted">Scan with a phone on the same network, then type the code of this device: the phone lends its workers while the page stays open.</p>`
        : '<p class="muted">This device has no address on the local network: phones cannot join it.</p>';
    return `${enabled}${failure(null)}${device}
        <section><h3>Nearby</h3><ul>${nearby}</ul></section>
        <section><h3>Linked</h3><ul>${linked}</ul></section>
        <section><h3>A phone</h3>${phone}</section>`;
}

/** Binds the view in [root] to [vscode], whose messages come to [page] */
export function connect(root: HTMLElement, vscode: VsCode, page: EventTarget): void {
    let state: FederationState | null = null;
    const codes = new Map<string, string>();
    let failure: Shown['failure'] = null;
    // a text being typed is left alone: the view is shown again once it is left
    let skipped = false;
    const typing = () => {
        const active = document.activeElement;
        return active instanceof HTMLInputElement && active.type !== 'checkbox' && root.contains(active);
    };
    const render = () => {
        skipped = typing();
        if (!skipped) root.innerHTML = view(state, { codes, failure });
    };
    root.addEventListener('focusout', () => setTimeout(() => skipped && render()));

    page.addEventListener('message', event => {
        const message = (event as MessageEvent<ToFederation>).data;
        if (message.type === 'state') {
            state = message.state;
            failure = null;
        } else failure = { site: message.site, message: message.message };
        render();
    });

    root.addEventListener('input', event => {
        const input = event.target as HTMLInputElement;
        if (input.dataset.site !== undefined) codes.set(input.dataset.site, input.value.trim());
    });
    root.addEventListener('change', event => {
        const input = event.target as HTMLInputElement;
        if (input.dataset.action === 'enable') vscode.postMessage({ type: 'enable', enabled: input.checked });
        else if (input.dataset.action === 'rename') {
            input.blur();
            vscode.postMessage({ type: 'rename', name: input.value.trim() });
        }
    });
    const pair = (site: string) => vscode.postMessage({ type: 'pair', site, code: codes.get(site) ?? '' });
    root.addEventListener('click', event => {
        const button = (event.target as HTMLElement).closest('button');
        if (button?.dataset.action === 'newCode') vscode.postMessage({ type: 'newCode' });
        else if (button?.dataset.action === 'pair' && button.dataset.site !== undefined) pair(button.dataset.site);
    });
    root.addEventListener('keydown', event => {
        const input = event.target as HTMLInputElement;
        if ((event as KeyboardEvent).key !== 'Enter') return;
        if (input.dataset.site !== undefined) pair(input.dataset.site);
        else if (input.dataset.action === 'rename') input.blur();
    });

    vscode.postMessage({ type: 'ready' });
}

// The name of [site], as [state] knows it
function nameOf(state: FederationState, site: string): string {
    return state.sites.find(it => it.id === site)?.name ?? state.nearby.find(it => it.id === site)?.name ?? site;
}

function plural(count: number, noun: string): string {
    return `${count} ${noun}${count === 1 ? '' : 's'}`;
}

function escape(text: string): string {
    return text.replace(/[&<>"']/g, it => `&#${it.charCodeAt(0)};`);
}
