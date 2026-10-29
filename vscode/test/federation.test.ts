import { assert, describe, it } from 'vitest';
import { Federation } from '../src/federation.js';
import type { FederationState, ToFederation } from '../src/protocol.js';
import { view } from '../webview/federation.js';

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

const state: FederationState = {
    enabled: true,
    name: 'desk',
    code: '0427',
    joinUrl: 'http://192.168.1.20:51234/',
    workers: 8,
    nearby: [{ id: 'site-a', name: 'laptop', requiresCode: true }],
    sites: [
        { id: 'site-b', name: 'lab', workers: 4, via: null },
        { id: 'site-c', name: null, workers: 3, via: null },
    ],
};

// A server answering the requests of the federation with [state], refusing a pairing with a wrong code
function server() {
    const asked: [string, unknown][] = [];
    return {
        asked,
        request: <T>(method: string, params: unknown): Promise<T> => {
            asked.push([method, params]);
            if (method === 'arg2p/federation/pair' && (params as { code: string }).code !== '1234') {
                return Promise.reject(new Error('site-a refused the link: Wrong code'));
            }
            return Promise.resolve(state as T);
        },
    };
}

describe('Federation', function () {
    it('should ask the server its state, pair and renew the code, and tell the view', async function () {
        const fake = server();
        const posted: ToFederation[] = [];
        const federation = new Federation(fake, message => posted.push(message), () => undefined);
        await federation.handle({ type: 'ready' });
        await federation.handle({ type: 'pair', site: 'site-a', code: '0000' });
        await federation.handle({ type: 'pair', site: 'site-a', code: '1234' });
        await federation.handle({ type: 'newCode' });
        deepEqual(fake.asked, [
            ['arg2p/federation', undefined],
            ['arg2p/federation/pair', { site: 'site-a', code: '0000' }],
            ['arg2p/federation/pair', { site: 'site-a', code: '1234' }],
            ['arg2p/federation/newCode', undefined],
        ]);
        deepEqual(posted, [
            { type: 'state', state },
            { type: 'failed', site: 'site-a', message: 'site-a refused the link: Wrong code' },
            { type: 'state', state },
            { type: 'state', state },
        ]);
    });

    it('should change the settings the view asks to, and tell the server the settings changed', async function () {
        const fake = server();
        const configured: [string, unknown][] = [];
        const federation = new Federation(fake, () => undefined, (setting, value) => configured.push([setting, value]));
        await federation.handle({ type: 'rename', name: 'lab' });
        await federation.handle({ type: 'enable', enabled: false });
        deepEqual(configured, [
            ['name', 'lab'],
            ['enabled', false],
        ]);
        equal(fake.asked.length, 0);
        await federation.apply({ enabled: false, name: 'lab' });
        deepEqual(fake.asked, [
            ['arg2p/federation/rename', { name: 'lab' }],
            ['arg2p/federation/enable', { enabled: false }],
        ]);
    });

    it('should tell the view there is no state while the server does not run', async function () {
        const posted: ToFederation[] = [];
        const federation = new Federation({ request: () => Promise.reject(new Error('not running')) }, message => posted.push(message), () => undefined);
        await federation.refresh();
        deepEqual(posted, [{ type: 'state', state: null }]);
    });
});

describe('The view of the federation', function () {
    const none = { codes: new Map<string, string>(), failure: null };

    it('should show this device, the devices nearby and linked, and the QR code of the page of a phone', function () {
        const html = view(state, { codes: new Map([['site-a', '12']]), failure: { site: 'site-a', message: 'Wrong <code>' } });
        ok(html.includes('value="desk"'));
        ok(html.includes('0427'));
        ok(html.includes('8 workers'));
        ok(html.includes('laptop') && html.includes('data-action="pair" data-site="site-a"'));
        // the code typed is kept, and a failure told where it happened, escaped
        ok(html.includes('value="12"'));
        ok(html.includes('Wrong &#60;code&#62;'));
        ok(html.includes('lab') && html.includes('4 workers') && html.includes('A browser'));
        ok(html.includes('<svg') && html.includes('http://192.168.1.20:51234/'));
    });

    it('should tell when it is disabled, the server does not run, or a phone cannot join', function () {
        const disabled = view({ ...state, enabled: false }, none);
        ok(disabled.includes('8 workers of this device alone'));
        ok(!disabled.includes('<svg'));
        ok(view(null, none).includes('not running'));
        ok(view({ ...state, joinUrl: null }, none).includes('phones cannot join'));
    });
});
